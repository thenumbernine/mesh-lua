local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local math = require 'ext.math'
local range = require 'ext.range'
local timer = require 'ext.timer'
local vector = require 'ffi.cpp.vector'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local box3f = require 'vec-ffi.box3f'
local matrix_ffi = require 'matrix.ffi'
matrix_ffi.real = 'float'

ffi.cdef[[
typedef struct {
	vec3f_t pos;
	vec3f_t texcoord;
	vec3f_t normal;

	// per-triangle stats (duplicated 3x per-vertex)
	// TODO move this to a separate buffer
	vec3f_t com;		//com of tri containing this vertex.  only good for un-indexed drawing.
} MeshVertex_t;
]]

local Mesh = class()

local function triArea(a,b,c)
	-- TODO check nans here?
	local ab = b - a
	local ac = c - a
	local n = ab:cross(ac)
	return .5 * n:norm()
end
Mesh.triArea = triArea

-- TODO the more I use this (and build off it ... giving triangle TNB frames ...)
-- the more I think I should re-introduce storing the triangle normal
local function triNormal(a,b,c)
	local ab = b - a
	local bc = c - b
	local n = ab:cross(bc)
	local len = n:norm()
	if len < 1e-7 or not math.isfinite(len) then
		return vec3f(0,0,0), len * .5
	else
		return n / len, len * .5	-- returns the unit normal, triangle area
	end
end
Mesh.triNormal = triNormal

local function triCOM(a,b,c)
	-- TODO check nans here?
	return (a + b + c) * (1/3)
end
Mesh.triCOM = triCOM

-- volume of parallelogram with vertices at 0, a, b, c
-- the 4th pt in the tetrad is zero.  adjust a,b,c accordingly
local function tetradVolume(a,b,c)
	return (a.x * b.y * c.z
		+ a.y * b.z * c.x
		+ a.z * b.x * c.y
		- c.x * b.y * a.z
		- c.y * b.z * a.x
		- c.z * b.x * a.y) / 6
end
Mesh.tetradVolume = tetradVolume

local function rayPlaneIntersect(rayPos, rayDir, planeNormal, planePt)
	-- ((rayPos + s * rayDir) - planePt) dot planeNormal = 0
	-- rayPos dot planeNormal + s * rayDir dot planeNormal - planePt dot planeNormal = 0
	-- s = ((planePt - rayPos) dot planeNormal) / (rayDir dot planeNormal)
	local s = (planePt - rayPos):dot(planeNormal) / rayDir:dot(planeNormal)
	return rayPos + rayDir * s, s
end

--[[ holds extra info per tri:
.index = what it's 1-based index is
.group = what its group is (unnecessary? finding via groups ranges is probably faster than finding a tri in the .tris table)
.com = triangle center-of-mass (average of 3 vtxs)
.area
.normal = tri surface normal (surface z-axis)
.basis = [3] of tangent, (negative of) binormal, normal

findEdges:
.edges

unwrapuv:
.uvs = [3] vec2f's of the uv's of the triangle (before breaking and writing)
.uvorigin2D
.uvorigin3D
--]]
local Triangle = class()

function Triangle:init(args)
	if args then
		for k,v in pairs(args) do
			self[k] = v
		end
	end
end

function Triangle:indexes(mesh)
	local ti = 3 * (self.index - 1)
	assert(ti >= 0 and ti + 3 <= mesh.triIndexes.size)
	local tp = mesh.triIndexes.v + ti
	local i,j,k = tp[0], tp[1], tp[2]
	assert(i >= 0 and i < mesh.vtxs.size)
	assert(j >= 0 and j < mesh.vtxs.size)
	assert(k >= 0 and k < mesh.vtxs.size)
	return i,j,k
end

function Triangle:vtxs(mesh)
	local i,j,k = self:indexes(mesh)
	return mesh.vtxs.v[i], mesh.vtxs.v[j], mesh.vtxs.v[k]
end

function Triangle:vtxPos(mesh)
	local va,vb,vc = self:vtxs(mesh)
	return va.pos, vb.pos, vc.pos
end

function Triangle:calcCOM(mesh)
	local a, b, c = self:vtxPos(mesh)
	self.com = (a + b + c) * (1 / 3)
end

function Triangle:calcAux(mesh)
	self:calcCOM(mesh)
	self.normal, self.area = mesh.triNormal(self:vtxPos(mesh))
end

-- calculate the barycentric coordinates of point 'p'
function Triangle:calcBCC(p, mesh)
	local ti = 3 * (self.index - 1)
	assert(ti >= 0 and ti + 3 <= mesh.triIndexes.size)
	local tp = mesh.triIndexes.v + ti
	assert(tp[0] >= 0 and tp[0] < mesh.vtxs.size)
	assert(tp[1] >= 0 and tp[1] < mesh.vtxs.size)
	assert(tp[2] >= 0 and tp[2] < mesh.vtxs.size)

	local bcc = vec3f()
	for j=0,2 do
		local v1 = mesh.vtxs.v[tp[j]].pos
		local v2 = mesh.vtxs.v[tp[(j+1)%3]].pos
		local tocom = (v2 - v1):cross(self.normal)
		-- TODO rescale correctly
		-- will only work on front-facing triangles
		bcc.s[j] = (p - v1):dot(tocom) < 0
	end
	return bcc
end

function Triangle:calcTetradVolume(mesh)
	return tetradVolume(self:vtxPos(mesh))
end

Mesh.Triangle = Triangle

function Mesh:init(o)
	-- TODO replace my lua-ization of cpp-vectors
	-- ...with a cdef-ization of lua-tables
	-- because everyone knows the stl api is way too longwinded compared to equiv commands in other languages/apis, and is only that way to accomodate functional programming and templates.
	self.vtxs = vector'MeshVertex_t'
	self.triIndexes = vector'int32_t'

	-- array of Triangle's
	self.tris = table()

	-- holds 0-based ranges of tris
	self.groups = table()
end

-- combines 'self' with other meshes.
-- operates in-place.
-- returns self
function Mesh:combine(...)
	self.mtlFilenames = table(self.mtlFilenames)

	for oi=1,select('#', ...) do
		local o = select(oi, ...)

		local firstVtx = self.vtxs.size
		self.vtxs:resize(self.vtxs.size + o.vtxs.size)
		ffi.copy(self.vtxs.v + firstVtx, o.vtxs.v, ffi.sizeof(o.vtxs.type) * o.vtxs.size)

		local firstIndex = self.triIndexes.size
		self.triIndexes:resize(self.triIndexes.size + o.triIndexes.size)
		for i=0,o.triIndexes.size-1 do
			self.triIndexes.v[firstIndex + i] = o.triIndexes.v[i] + firstVtx
		end

		local firstGroup = #self.groups+1
		self.groups:append(o.groups:mapi(function(g)
			g = table(g):setmetatable(nil)
			g.triFirstIndex = g.triFirstIndex + firstIndex/3
			return g
		end))

		self.tris:append(o.tris:mapi(function(t)
			t = table(t):setmetatable(nil)
			local groupIndex = o.groups:find(t.group)
			t.group = groupIndex and self.groups[firstGroup + groupIndex - 1] or nil
			return t
		end))
		for i,t in ipairs(self.tris) do
			t.index = i
		end

		self.mtlFilenames:append(o.mtlFilenames)
	end

	self.mtlFilenames = self.mtlFilenames:mapi(function(v,k,t)
		return true, v
	end):map(function(v,k,t)
		return k, #t+1
	end)

	self.edges = nil
	self.edgeIndexBuf = nil
	self.loadedGL = nil
	self.vtxBuf = nil

	return self
end

function Mesh:clone()
	return Mesh():combine(self)
end

-- TODO operators?  * number, * vec3f, etc?
function Mesh:scale(...)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i].pos
		for j=0,2 do
			v.s[j] = v.s[j] * select(j+1, ...)
		end
	end
	self:refreshVtxs()
	return self
end

-- TODO operators?
function Mesh:translate(...)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i].pos
		for j=0,2 do
			v.s[j] = v.s[j] + select(j+1, ...)
		end
	end
	self:refreshVtxs()
	return self
end

-- quaternion?  matrix?  angle-axis? detect?
-- quaternion for now.
function Mesh:rotate(q)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i].pos
		for j=0,2 do
			v.s[j] = q:rotate(v.s[j])
		end
	end
	self:refreshVtxs()
	return self
end

function Mesh:refreshVtxs()
	if self.loadedGL then
		self.vtxBuf:updateData(0, ffi.sizeof'MeshVertex_t' * self.vtxs.size, self.vtxs.v)
	end
	self.bbox = nil
	-- TODO invalidate instead of recalculate?
	--self:findEdges()
	--self:calcCOMs()
	return self
end

function Mesh:prepare()
-- [[ calculate bbox.
-- do this before merging vertexes
	self:calcBBox()
--]]
-- TODO maybe calc bounding radius? Here or later?  That takes COM, which, for COM2/COM3 takes tris.  COM1 takes edges... should COM1 consider merged edges always?  probably...

	-- store all edges of all triangles
	-- ... why?  who uses this?
	-- unwrapUVs used to but now it uses the 'allOverlappingEdges' structure
	-- it's used for visualization
	self:findEdges()

	-- calculate coms ...
	self:calcCOMs()
end

function Mesh:calcBBox()
	self.bbox = box3f.empty()
	for i=0,self.vtxs.size-1 do
		self.bbox:stretch(self.vtxs.v[i].pos)
	end
end

--[=[
args:
	prec = precision, default 1e-5
	posPrec = precision for testing .pos uniqueness.  nil means no precision means don't even check.
	texCoordPrec = use .texcoord to test uniqueness.
	normalPrec = use .normal to test uniqueness.
	usedIndexes = optional map {[0-based-vtx-index] = true} to flag which indexes to check

returns:
	uniquevs = lua-table holding all the unique 0-based vtx indexes
	indexToUniqueV = {[0-based-vtx-index] = 1-based index in uniquevs} = map from old (0-based c-array) to new (1-based lua-table)

it should always be the case that uniquevs[indexToUniqueV[i]] <= i
--]=]
function Mesh:getUniqueVtxs(posPrec, texCoordPrec, normalPrec, usedIndexes)
	local function vec3ToStrPrec(v, prec)
		return tostring(v:map(function(x)
			return math.round(x / prec) * prec
		end))
	end

	-- map from the vtxs to unique indexes
	local uniquevs = table()

	-- used by tris.
	-- map from all vtxs.v[], into unique indexes
	-- rounds values to precision 'prec'
	-- keys are 0-based, values are 1-based
	local indexToUniqueV = {}

	-- maps from a key (from rounded vec3f) to uniquevs index
	-- goes a *lot* faster than the old way
	local keyToUnique = {}

	for i=0,self.vtxs.size-1 do
		if not usedIndexes or usedIndexes[i] then
			local v = self.vtxs.v[i]
			local k = table{
				posPrec and vec3ToStrPrec(v.pos, posPrec) or '',
				texCoordPrec and vec3ToStrPrec(v.texcoord, texCoordPrec) or '',
				normalPrec and vec3ToStrPrec(v.normal, normalPrec) or ''
			}:concat','
			local j = keyToUnique[k]
			if j then
				indexToUniqueV[i] = j
			else
				uniquevs:insert(i)
				keyToUnique[k] = #uniquevs
				indexToUniqueV[i] = #uniquevs
			end
		end
	end
	return uniquevs, indexToUniqueV
end

function Mesh:mergeMatchingVertexes(skipTexCoords, skipNormals)
	assert(#self.tris*3 == self.triIndexes.size)
	if not self.bbox then self:calcBBox() end
	-- ok the bbox hyp is 28, the smallest maybe valid dist is .077, and everything smalelr is 1e-6 ...
	-- that's a jump from 1/371 to 1/20,000,000
	-- so what's the smallest ratio I should allow?  maybe 1/1million?
	local bboxCornerDist = (self.bbox.max - self.bbox.min):norm()
	local vtxMergeThreshold = bboxCornerDist * 1e-6
print('vtxMergeThreshold', vtxMergeThreshold)
print('before merge vtx count', self.vtxs.size, 'tri count', self.triIndexes.size)

	local uniquevs, indexToUniqueV = self:getUniqueVtxs(
		vtxMergeThreshold,
		not skipTexCoords and 1e-7,
		not skipNormals and 1e-7
	)
	for i=self.vtxs.size-1,1,-1 do
		local j = uniquevs[indexToUniqueV[i]]
		assert(j <= i)
		if j < i then
			self:mergeVertex(i,j)
		end
	end
print('after merge vtx count', self.vtxs.size, 'tri count', self.triIndexes.size)
	assert(#self.tris*3 == self.triIndexes.size)

	-- invalidate
	self.loadedGL = false
	self.vtxBuf = nil
	self.vtxAttrs = nil
	self.vao = nil

	self.edges = nil
	self.edgeIndexBuf = nil
	self.allOverlappingEdges = nil
end

-- 0-based, index-array so 3x from unique tri
function Mesh:triVtxPos(i)
	local t = self.triIndexes.v + i
	return self.vtxs.v[t[0]].pos,
			self.vtxs.v[t[1]].pos,
			self.vtxs.v[t[2]].pos
end

function Mesh:removeEmptyTris()
	for i,t in ipairs(self.tris) do
		if t.area < 1e-7 then
			self:removeTri(3*(i-1))
		end
	end
end

-- rebuild .tris from .triIndexes
function Mesh:rebuildTris(from,to)
	if not from then
		from = 1
		to = self.triIndexes.size/3
	end
	for i=from,to do
		if not self.tris[i] then
			self.tris[i] = Triangle()
		end
		self.tris[i].index = i
		self.tris[i]:calcAux(self)
	end
	assert(#self.tris*3 == self.triIndexes.size)
	for i,t in ipairs(self.tris) do
		assert(Triangle:isa(t))
	end
end

-- i know "TNB" is the traditional, cuz thats the order you calculate them in the Frenet frame
-- but if "normal" is the surface dir (which I'm making the 'Z' axis) so that u and v in 2D align with x and y in 3D ...
-- then it becomes tangent-(negative)binormal-normal
function Mesh:clearTriBasis(mesh)
	for i,t in ipairs(mesh.tris) do
		-- TODO unify this with .normal
		t.basis = nil
	end
end

-- generate tangent, binormal, normal
-- TODO merge normal with this
-- TODO TODO merge position with this so that 'pos normal texcoord-u texcoord-v' are a basis
-- pos should go first, not last.  just like time should go first, not last.
-- because ofc pos = eps rot around inf origin = integral of time-velocity = integral of exp map of Lorentz boost-generators (from acceleration)
function Mesh:generateTriBasis()
	--[[ make sure triangles have basis
	vectors are columns ...
	[T|B]' * (pos[i] - pos0) = tc[i] - tc0
	[T|B] = 3x2, ' is transpose is 2x3
	let dpos = pos[i] - pos0, so it is 3x3 with 1rd row 0
	... same with tc
	... and we can truncate those 0 rows
	[T|B] * [T|B]' * dpos = [T|B] * dtc
	[T|B] * [T|B]' = I 2x2 since T and B are orthogonal ... but not vice versa since [T|B] is 3x2
	dpos * dtc^-1 = [T|B] * dtc * dtc^-1
	[T|B] = dpos * dtc^-1
	--]]
	for i,t in ipairs(self.tris) do
		if not t.basis then
			local ti = 3*(i-1)
			assert(ti >= 0 and ti < self.triIndexes.size)
			local tp = self.triIndexes.v + ti
			assert(tp[0] >= 0 and tp[0] < self.vtxs.size)
			assert(tp[1] >= 0 and tp[1] < self.vtxs.size)
			assert(tp[2] >= 0 and tp[2] < self.vtxs.size)
			local va = self.vtxs.v[tp[0]]
			local vb = self.vtxs.v[tp[1]]
			local vc = self.vtxs.v[tp[2]]
			local dpos1 = vb.pos - va.pos
			local dpos2 = vc.pos - va.pos
			local dtc1 = vb.texcoord - va.texcoord	-- only considering 2D of it
			local dtc2 = vc.texcoord - va.texcoord
			-- dtc is matrix with columns of dtc[i]
			local dtc = matrix_ffi{
				{dtc1.x, dtc2.x},
				{dtc1.y, dtc2.y},
			}
			-- now 2x2 invert
			local dtcInv = dtc:inv()
			--assert((dtc * dtcInv - matrix_ffi{{1,0},{0,1}}):normSq() < 1e-7)

			-- get the cols
			local dtcInv1 = vec2f(dtcInv[1][1], dtcInv[2][1])
			local dtcInv2 = vec2f(dtcInv[1][2], dtcInv[2][2])

			local n = dpos1:cross(dpos2):normalize()

			local ex = vec3f(
				dtcInv1:dot(vec2f(dpos1.x, dpos2.x)),
				dtcInv1:dot(vec2f(dpos1.y, dpos2.y)),
				dtcInv1:dot(vec2f(dpos1.z, dpos2.z))
			):normalize()
			--[[ don't use ey ... just use N x ex...
			local ey = vec3f(
				dtcInv2:dot(vec2f(dpos1.x, dpos2.x)),
				dtcInv2:dot(vec2f(dpos1.y, dpos2.y)),
				dtcInv2:dot(vec2f(dpos1.z, dpos2.z))
			):normalize()
			--]]
			--[[ or use the delta as ex ...
			local ex = dpos1:normalize()
			--]]
			-- [[ orthogonalize
			local ey = n:cross(ex):normalize()
			--]]
			t.basis = {ex, ey, n}
--print(i, table.unpack(t.basis), n:dot(ex), n:dot(ey))
		end
	end
end




-- fill the allOverlappingEdges table
-- TODO instead generate this upon request?
function Mesh:calcAllOverlappingEdges()
	--[[
	these are whatever mesh edges are partially overlapping one another.
	they are a result of a shitty artist.
	because of which, there is no guarantee with this table that each tri has 3 edges, and each edge has only 2 tris.
	instead it's a shitfest shitstorm.
	--]]
	self.allOverlappingEdges = table()
	for _,t in ipairs(self.tris) do
		t.allOverlappingEdges = table()
	end
--for _,t in ipairs(self.tris) do
--	print('n = '..t.normal)
--end
	for i1=#self.tris,2,-1 do
		local tp1 = self.triIndexes.v + 3 * (i1 - 1)
		for j1=1,3 do
			-- t1's j1'th edge
			local v11 = self.vtxs.v[tp1[j1-1]].pos
			local v12 = self.vtxs.v[tp1[j1%3]].pos
--print('tri', i1, 'pos', j1, '=', v11)
			local n1 = v12 - v11
			local n1NormSq = n1:normSq()
			if n1NormSq  > 1e-3 then
				n1 = n1 / math.sqrt(n1NormSq)
				for i2=i1-1,1,-1 do
					local t2 = self.tris[i2]
					local tp2 = self.triIndexes.v + 3 * (i2 - 1)
					for j2=1,3 do
						local v21 = self.vtxs.v[tp2[j2-1]].pos
						local v22 = self.vtxs.v[tp2[j2%3]].pos
						local n2 = v22 - v21
						local n2NormSq = n2:normSq()
						if n2NormSq  > 1e-3 then
							n2 = n2 / math.sqrt(n2NormSq)
							if math.abs(n1:dot(n2)) > 1 - 1e-3 then
--print('allOverlappingEdges normals align:', i1-1, j1-1, i2-1, j2-1)
								-- normals align, calculate distance
								local rayPos = v11	-- pick any point on line v1: v11 or v12
								local rayDir = n1
								local dv = v21 - rayPos	-- ray from the v1 line to any line on v2
								dv = dv - rayDir * dv:dot(rayDir)		-- project onto the plane normal
								local dist = dv:norm()
								if dist < 1e-3 then
									-- now find where along plane normal the intervals {v11,v12} and {v21,v22}
									local s11 = 0	--(v11 - rayPos):dot(rayDir) -- assuming v11 is the plane origin
									local s12 = (v12 - rayPos):dot(rayDir)
									-- based on n1 being the plane normal, s11 and s12 are already sorted
									local s21 = (v21 - rayPos):dot(rayDir)
									local s22 = (v22 - rayPos):dot(rayDir)
									-- since these aren't, they have to be sorted
									if s21 > s22 then s21, s22 = s22, s21 end
									if s11 < s22 and s12 > s21 then
										-- in my loop i2 < i1, but i want it ordered lowest-first, so ... swap them
										assert(i2 < i1)
										local t1 = self.tris[i1]
										local t2 = self.tris[i2]
										local e = {
											tris = {t2, t1},
											triVtxIndexes = {j2, j1},
											intervals = {{s21,s22}, {s11,s12}},
											dist = dist,
											rayPos = rayPos,
											rayDir = rayDir
										}
										self.allOverlappingEdges:insert(e)
										t1.allOverlappingEdges:insert(e)
										t2.allOverlappingEdges:insert(e)
									end
								end
							end
						end
					end
				end
			end
		end
	end
--[[
	for _,e in ipairs(self.allOverlappingEdges) do
		print(
			'edges', self.tris:find(e.tris[1])-1, e.triVtxIndexes[1]-1,
			'and', self.tris:find(e.tris[2])-1, e.triVtxIndexes[2]-1,
			'align with dist', e.dist,
			'with projected intervals', table.concat(e.intervals[1], ', '),
			'and', table.concat(e.intervals[2], ', '))
	end
	print('found', #self.allOverlappingEdges, 'overlaps')
--]]
end

function Mesh:findEdges(getIndex)
	if not getIndex then getIndex = function(a) return a end end
	-- and just for kicks, track all edges
	if not self.edgeIndexBuf then
		self.edgeIndexBuf = vector('int32_t', 6 * #self.tris)
	end
	self.edgeIndexBuf:resize(0)

	assert(#self.tris*3 == self.triIndexes.size)
	for i=1,self.triIndexes.size/3 do
		local t = self.tris[i]
		if not Triangle:isa(t) then
			error("got a bad tri at "..i..": "..require 'ext.tolua'(t))
		end
	end
	timer('edges', function()
		self.edges = {}
		local function addEdge(a,b,t)
			if a > b then return addEdge(b,a,t) end
			self.edges[a] = self.edges[a] or {}
			local e = self.edges[a][b]
			if not e then
				-- new edge?  add it to the index buffer
				self.edgeIndexBuf:push_back(a-1)
				self.edgeIndexBuf:push_back(b-1)
				e = {
					[1] = a,
					[2] = b,
					tris = table(),
					length = (self.vtxs.v[a-1].pos - self.vtxs.v[b-1].pos):norm(),
				}
				self.edges[a][b] = e
			end
			e.tris:insert(t)
			t.edges:insert(e)
		end
		for i=0,self.triIndexes.size-1,3 do
			local tp = self.triIndexes.v + i
			local a = getIndex(tp[0])
			local b = getIndex(tp[1])
			local c = getIndex(tp[2])
			assert(i/3+1 >= 0 and i/3+1 <= #self.tris)
			local ti = i/3+1
			local t = self.tris[ti]
			if not Triangle:isa(t) then
				error("got a bad tri at "..ti..": "..require 'ext.tolua'(t))
			end
			t.edges = table()
			addEdge(a+1, b+1, t)
			addEdge(a+1, c+1, t)
			addEdge(b+1, c+1, t)
		end
	end)
end

--[[
calculate and store COMs
TODO store these?  or only calculate upon demand?
this will have to be recalculated every time the mesh changes
a prereq for calcCOM1 is findEdges()
--]]
function Mesh:calcCOMs()
	timer('com0', function()
		self.com0 = self:calcCOM0()
	end)
	print('com0 = '..self.com0)
	timer('com1', function()
		self.com1 = self:calcCOM1()
	end)
	print('com1 = '..self.com1)
	timer('com2', function()
		self.com2 = self:calcCOM2()
	end)
	print('com2 = '..self.com2)
	timer('com3', function()
		self.com3 = self:calcCOM3()
	end)
	print('com3 = '..self.com3)
	-- can only do this with com2 and com3 since they use tris, which are stored per-material
	-- ig i could with edges and vertexes too if I flag them per-material
	timer('group com2/3', function()
		for _,g in ipairs(self.groups) do
			g.com2 = self:calcCOM2(g.name)
			g.com3 = self:calcCOM3(g.name)
		end
	end)
end

-- replace all instances of one vertex index with another
function Mesh:replaceVertex(from,to)
--print('replacing vertex ' ..from..' with '..to)
	assert(from > to)
	assert(from >= 0 and from <= self.vtxs.size)
	assert(to >= 0 and to <= self.vtxs.size)
	-- replace in .tris
	for j=self.triIndexes.size-3,0,-3 do
		local t = self.triIndexes.v + j
		for i=0,2 do
			if t[i] == from then t[i] = to end
		end
	end
end

function Mesh:removeDegenerateTriangles()
	for i=self.triIndexes.size-3,0,-3 do
		local t = self.triIndexes.v + i
		for j=2,1,-1 do
			if t[j] == t[j-1] then
--print('removing degenerate tri '..i..' with duplicate vertices')
				self:removeTri(i)
				break
			end
		end
	end
end

-- index is 0-based in increments of 3
function Mesh:removeTri(i)
	assert(#self.tris*3 == self.triIndexes.size)
	self.triIndexes:erase(self.triIndexes.v + i, self.triIndexes.v + i + 3)
	for _,g in ipairs(self.groups) do
		if i < g.triFirstIndex then
			g.triFirstIndex = g.triFirstIndex - 1
		elseif i >= g.triFirstIndex and i < g.triFirstIndex + g.triCount then
			g.triCount = g.triCount - 1
		end
	end
	-- phasing this out
	self.tris:remove(i/3+1)
	for j=i/3+1,#self.tris do
		self.tris[j].index = j
	end
end

-- remove all instances of a veretx index
-- remove the vertex from the elf.vs[] list
-- decrement the indexes greater
function Mesh:removeVertex(vi)
	assert(vi >= 0 and vi < self.vtxs.size)
	self.vtxs:erase(self.vtxs.v + vi, self.vtxs.v + vi + 1)
	-- remove in .tris
	-- if you did :replaceVertex and :removeDegenerateFaces first then the rest shouldn't be necessary at all (except for error checking)
	-- if you just straight up remove a vertex then the tris and faces might go out of sync
	for j=self.triIndexes.size-3,0,-3 do
		local t = self.triIndexes.v + j
		for i=0,2 do
			if t[i] == vi then
				--error("found a to-be-removed vertex index in a tri.  you should merge it first, or delete tris containing it first.")
				self:removeTri(j)
				break
			elseif t[i] > vi then
				t[i] = t[i] - 1
			end
		end
	end
end

-- TODO just use a single dense tri array
-- don't use indexes at all

--[[
1) replace the 'from' with the 'to'
2) remove any degenerate triangles/faces
3) remove the to vertex from the list

-- TODO same for .vts and .vns ?
--]]
function Mesh:mergeVertex(from,to)
	assert(from > to)
	self:replaceVertex(from,to)
	self:removeDegenerateTriangles()
	self:removeVertex(from)
end

function Mesh:removeUnusedVtxs()
	local usedVs = {}
	timer('finding used vertexes', function()
		for i=0,self.triIndexes.size-1 do
			usedVs[self.triIndexes.v[i]] = true
		end
	end)
	timer('removing unused vertexes', function()
		print('before removing, #vs', self.vtxs.size)
		for i=self.vtxs.size-1,0,-1 do
			if not usedVs[i] then
				self:removeVertex(i)
			end
		end
		print('after removing, #vs', self.vtxs.size)
	end)
end

-- common interface?  for dif 3d format types?
function Mesh:vtxiter()
	return coroutine.wrap(function()
		for i,v in ipairs(self.vs) do
			coroutine.yield(v)
		end
	end)
end

function Mesh:getTriIndexesForMaterial(groupname)
	if groupname then
		local _, g = self.groups:find(nil, function(g) return g.name == groupname end)
		if g then
			return g.triFirstIndex, g.triFirstIndex + g.triCount - 1
		else
			return 0, -1
		end
	else
		return 0, self.triIndexes.size/3-1
	end
end

-- yields with each material collection for a particular material name
-- default = no name = iterates over all materials
function Mesh:groupiter(groupname)
	return coroutine.wrap(function()
		if groupname then
			local _, g = self.groups:find(nil, function(g) return g.name == groupname end)
			if g then coroutine.yield(g, g.name) end
		else
			for _,g in ipairs(self.groups) do
				coroutine.yield(g, g.name)
			end
		end
	end)
end

-- calculate COM by 0-forms (vertexes)
function Mesh:calcCOM0()
	local result = vec3f()
	for i=0,self.vtxs.size-1 do
		result = result + self.vtxs.v[i].pos
	end
	result = result / self.vtxs.size
	if not math.isfinite(result:normSq()) then
io.stderr:write("couldn't even find the com0\n")
		return vec3f()
	end
	return result
end

-- calculate COM by 1-forms (edges)
-- depend on self.edges being stored
function Mesh:calcCOM1()
	local totalCOM = vec3f()
	local totalLen = 0
	for a,bs in pairs(self.edges) do
		for b in pairs(bs) do
			local v1 = self.vtxs.v[a-1].pos
			local v2 = self.vtxs.v[b-1].pos
			-- volume = *<Q,Q> = *(Q∧*Q) where Q = (b-a)
			-- for 1D, volume = |b-a|
			local length = (v1 - v2):norm()
			local com = (v1 + v2) * .5
			totalCOM = totalCOM + com * length
			totalLen = totalLen + length
		end
	end
	if totalLen == 0 then
		return self:calcCOM0()
	end
	local result = totalCOM / totalLen
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculate COM by 2-forms (triangles)
-- volume = *<Q,Q> = *(Q∧*Q) where Q = (b-a) ∧ (c-a)
-- for 2D, volume = |(b-a)x(c-a)|
function Mesh:calcCOM2(groupname)
	local totalCOM = vec3f()
	local totalArea = 0
	local i1, i2 = self:getTriIndexesForMaterial(groupname)
	for i=i1,i2 do
		local t = self.tris[i+1]
		totalCOM = totalCOM + t.com * t.area
		totalArea = totalArea + t.area
	end
	if totalArea == 0 then
		return self:calcCOM1(groupname)
	end
	local result = totalCOM / totalArea
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculate COM by 3-forms (enclosed volume)
function Mesh:calcCOM3(groupname)
	local totalCOM = vec3f()
	local totalVolume = 0
	local i1, i2 = self:getTriIndexesForMaterial(groupname)
	for i=i1,i2 do
		local t = self.tris[i+1]
		-- using [a,b,c,0] as the 4 pts of our tetrahedron
		-- volume = *<Q,Q> = *(Q∧*Q) where Q = (a-0) ∧ (b-0) ∧ (c-0)
		-- for 3D, volume = det|a b c|
		--local com = (a + b + c) * (1/4)
		local com = t.com * (3/4)

		local volume = t:calcTetradVolume(self)
		totalCOM = totalCOM + com * volume
		totalVolume = totalVolume + volume
	end
	-- if there's no volume then technically this can't exist ... but just fallback
	if totalVolume == 0 then
		return self:calcCOM2(groupname)
	end
	local result = totalCOM / totalVolume
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculates volume bounded by triangles
function Mesh:calcVolume()
	local totalVolume = 0
	for i,t in ipairs(self.tris) do
		totalVolume = totalVolume + t:calcTetradVolume(self)
	end
	if totalVolume < 0 then totalVolume = -totalVolume end
	return totalVolume
end

function Mesh:clearVertexNormals()
	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].normal:set(0,0,0)
	end
end

function Mesh:breakTriangles()
	print('before breakTriangles, #vtxs '..self.vtxs.size..' #triindexes '..self.triIndexes.size)
	local nvtxs = vector('MeshVertex_t', self.triIndexes.size)
	local ntris = vector('uint32_t', self.triIndexes.size)
	for i=0,self.triIndexes.size-1 do
		nvtxs.v[i] = self.vtxs.v[self.triIndexes.v[i]]
		ntris.v[i] = i
	end
	self.vtxs = nvtxs
	self.triIndexes = ntris
	print('after breakTriangles, #vtxs '..self.vtxs.size..' #triindexes '..self.triIndexes.size)

	-- TODO update the mesh ranges as well
	-- assert they do not overlap before
	-- then sort them
	-- then remap them as we break tris

	for i,t in ipairs(self.tris) do
		-- update vertex COMs
		-- they are only valid now
		local com = t.com
		for j=0,2 do
			nvtxs.v[3*(i-1)+j].com = com
		end
	end

	-- tell the next draw to regen the buffer
	-- can I resize a gl arraybuffer?
	self.loadedGL = false
	self.vtxBuf = nil
	self.vtxAttrs = nil
	self.vao = nil

	self.edges = nil
	self.edgeIndexBuf = nil
	self.allOverlappingEdges = nil

	self:calcBBox()
	self:findEdges()
	self:calcCOMs()
end

-- regenerate the vertex normals based on the face normals, weighted average by angle (tesselation-independent and curvature-driven)
function Mesh:generateVertexNormals()
	-- calculate vertex normals
	-- TODO store this?  in its own self.vn2s[] or something?
--print('zeroing vertex normals')
	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].normal:set(0,0,0)
	end
--print('accumulating triangle normals into vertex normals')
	for i=0,self.triIndexes.size-1,3 do
		local ia = self.triIndexes.v[i]
		local ib = self.triIndexes.v[i+1]
		local ic = self.triIndexes.v[i+2]
		-- not sure what i'm doing with these ...
		-- cache or regen?
		local va = self.vtxs.v[ia]
		local vb = self.vtxs.v[ib]
		local vc = self.vtxs.v[ic]
		local pa = self.vtxs.v[ia].pos
		local pb = self.vtxs.v[ib].pos
		local pc = self.vtxs.v[ic].pos
		local ab = (pb - pa):normalize()
		local bc = (pc - pb):normalize()
		local ca = (pa - pc):normalize()
		local normal = ab:cross(bc):normalize()
		local thetaA = math.acos(math.clamp(-ab:dot(ca),-1,1))
		local thetaB = math.acos(math.clamp(-bc:dot(ab),-1,1))
		local thetaC = math.acos(math.clamp(-ca:dot(bc),-1,1))
		va.normal = va.normal + normal * thetaA
		vb.normal = vb.normal + normal * thetaB
		vc.normal = vc.normal + normal * thetaC
	end
--print('normals vertex normals')
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i]
		local len = v.normal:norm()
		if len > 1e-7 then
			v.normal = v.normal * (1 / len)
		else
			v.normal:set(0,0,0)
		end
--print(k, vtxnormals[i])
	end

	if self.vtxBuf then
		self.vtxBuf:updateData(0, ffi.sizeof'MeshVertex_t' * self.vtxs.size, self.vtxs.v)
	end
end

function Mesh:recenter(newOrigin)
	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].pos = self.vtxs.v[i].pos - newOrigin
	end
	if self.vtxBuf then
		self.vtxBuf:updateData(0, ffi.sizeof'MeshVertex_t' * self.vtxs.size, self.vtxs.v)
	end
	-- recalculate coms
	self:calcCOMs()
end

-- all the draw functionality is tied tightly with view.lua so ...
-- idk if i should move it from one or the other


-- upon ctor the images are loaded (in case the caller isn't using GL)
-- so upon first draw - or upon manual call - load the gl textures
function Mesh:loadGL(shader)
	if self.loadedGL then return end
	self.loadedGL = true

	local gl = require 'gl'
	local glreport = require 'gl.report'
	local GLTex2D = require 'gl.tex2d'
	local GLArrayBuffer = require 'gl.arraybuffer'
	local GLAttribute = require 'gl.attribute'
	local GLVertexArray = require 'gl.vertexarray'

	-- load textures
	for _,g in ipairs(self.groups) do
		if g.image_Kd
		and not g.tex_Kd
		then
			g.tex_Kd = GLTex2D{
				image = g.image_Kd,
				minFilter = gl.GL_NEAREST,
				magFilter = gl.GL_LINEAR,
			}
		end
	end

--print('creating array buffer of size', self.vtxs.size)
	if not self.vtxBuf then
		self.vtxBuf = GLArrayBuffer{
			size = self.vtxs.size * ffi.sizeof'MeshVertex_t',
			data = self.vtxs.v,
			usage = gl.GL_STATIC_DRAW,
		}
		assert(glreport'here')

		self.vtxAttrs = table{
			{name='pos', size=3},
			{name='texcoord', size=3},
			{name='normal', size=3},
			{name='com', size=3},
		}:mapi(function(info)
			if not shader.attrs[info.name] then return end
			return GLAttribute{
				buffer = self.vtxBuf,
				size = info.size,
				type = gl.GL_FLOAT,
				stride = ffi.sizeof'MeshVertex_t',
				offset = ffi.offsetof('MeshVertex_t', info.name),
			}, info.name
		end)
		shader:use()
		assert(glreport'here')
		self.vao = GLVertexArray{
			program = shader,
			attrs = self.vtxAttrs,
		}
		shader:setAttrs(self.vtxAttrs)
		shader:useNone()
		assert(glreport'here')
	end
end

-- 'fwd' is used for depth calculation, 'dir' is the ray direction
function Mesh:findClosestVertexToMouseRay(pos, dir, fwd, cosEpsAngle)
	-- assumes dir is 1 unit fwd along the view fwd
	--dir = dir:normalize()
	local dirlen = dir:norm()
	local bestdot, besti, bestdepth
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i].pos
		local delta = v - pos
		local depth = delta:dot(fwd)
		--local dot = dir:dot(delta) / (delta:norm() * dirlen)
		local dot = dir:unit():dot(delta:unit())
		if dot > cosEpsAngle then
			if not bestdepth
			or depth < bestdepth
			then
				besti = i
				bestdepth = depth
				bestdot = dot
			end
		end
	end
	return besti, bestdepth
end

function Mesh:findClosestTriToMouseRay(pos, dir, fwd, cosEpsAngle)
	-- assumes dir is 1 unit fwd along the view fwd
	--dir = dir:normalize()
	local dirlen = dir:norm()
	local besti, bestdist
	assert(#self.tris * 3 == self.triIndexes.size)
	for ti,t in ipairs(self.tris) do
		local i = 3*(ti-1)
		local tnormal, area = t.normal, t.area

		local a,b,c = t:vtxPos(self)
		local planePt = a
		if area > 1e-7 then
			-- make sure it's pointing towards the ray origin
			if tnormal:dot(dir) < 0 then tnormal = -tnormal end

			local p, s = rayPlaneIntersect(pos, dir, tnormal, planePt)
			if s >= 0 and (not bestdist or s < bestdist) then
				-- barycentric coordinates
				local bcc = t:calcBCC(p, self)
				if bcc.x > 0 and bcc.y > 0 and bcc.z > 0 then
					besti = i
					bestdist = s
				end
			end
		end
	end
	return besti, bestdist
end

function Mesh:draw(args)
	local gl = require 'gl'

	self:loadGL()	-- load if not loaded

	local curtex
	for _,g in ipairs(self.groups) do
		--[[
		if g.Kd then
			gl.glColor4f(g.Kd:unpack())
		else
			gl.glColor4f(1,1,1,1)
		end
		--]]
		--[[
		if g
		and g.tex_Kd
		and not (args and args.disableTextures)
		then
			-- TODO use .Ka, Kd, Ks, Ns, etc
			-- with fixed pipeline?  opengl lighting?
			-- with a shader in the wavefrontobj lib?
			-- with ... nothing?
			curtex = g.tex_Kd
			curtex:enable()
			curtex:bind()
		else
			if curtex then
				curtex:unbind()
				curtex:disable()
				curtex = nil
			end
		end
		--]]
		if args.beginGroup then args.beginGroup(g) end

		--[[ immediate mode
		gl.glBegin(gl.GL_TRIANGLES)
		for vi in self:triindexiter(g.name) do
			-- TODO store a set of unique face v/vt/vn index-vertexes
			-- and then bake those into a unique vertex array, and store its index alongside face's other indexes
			-- that'll be most compat with GL indexed arrays
			local v = self.vtxs.v[vi]
			gl.glTexCoord2fv(v.texcoord.s)
			gl.glNormal3fv(v.normal.s)
			gl.glVertex3fv(self.vtxs.v[vi].pos.s)
		end
		gl.glEnd()
		--]]
		--[[ vertex client arrays
		gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', g.vtxs.v[0].pos.s)
		gl.glTexCoordPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', g.vtxs.v[0].texcoord.s)
		gl.glNormalPointer(gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', g.vtxs.v[0].normal.s)
		gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
		gl.glEnableClientState(gl.GL_TEXTURE_COORD_ARRAY)
		gl.glEnableClientState(gl.GL_NORMAL_ARRAY)
		gl.glDrawArrays(gl.GL_TRIANGLES, 0, g.vtxs.size)
		gl.glDisableClientState(gl.GL_VERTEX_ARRAY)
		gl.glDisableClientState(gl.GL_TEXTURE_COORD_ARRAY)
		gl.glDisableClientState(gl.GL_NORMAL_ARRAY)
		--]]
		--[[ vertex attrib pointers ... requires specifically-named attrs in the shader
		gl.glVertexAttribPointer(args.shader.attrs.pos.loc, 3, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'MeshVertex_t', g.vtxs.v[0].pos.s)
		gl.glVertexAttribPointer(args.shader.attrs.texcoord.loc, 3, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'MeshVertex_t', g.vtxs.v[0].texcoord.s)
		gl.glVertexAttribPointer(args.shader.attrs.normal.loc, 3, gl.GL_FLOAT, gl.GL_TRUE, ffi.sizeof'MeshVertex_t', g.vtxs.v[0].normal.s)
		gl.glEnableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.texcoord.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.normal.loc)
		gl.glDrawArrays(gl.GL_TRIANGLES, 0, g.vtxs.size)
		gl.glDisableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.texcoord.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.normal.loc)
		--]]
		-- [[ vao ... getting pretty tightly coupled with the view.lua file ...
		if g.triCount > 0 then
			self.vao:use()
			gl.glDrawElements(gl.GL_TRIANGLES, g.triCount * 3, gl.GL_UNSIGNED_INT, self.triIndexes.v + g.triFirstIndex * 3)
			self.vao:useNone()
		end
		--]]
		if args.endGroup then args.endGroup(g) end
	end
	--[[
	if curtex then
		curtex:unbind()
		curtex:disable()
	end
	--]]
	require 'gl.report''here'
end

-- make sure my edges match my faces
-- can't handle group explode dist because edges aren't stored associted with materials ...
-- they are per-tri, which is per-face, which is per-material, but there can be multiple materials per edge.
function Mesh:drawEdges(triExplodeDist, groupExplodeDist)
	local gl = require 'gl'

	if not self.edgeIndexBuf then
		self:findEdges()
	end

	--gl.glLineWidth(3)
	gl.glColor3f(1,1,0)

	-- TODO shader that does the explode stuff
	gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].pos.s)
	gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
	gl.glDrawElements(gl.GL_LINES, self.edgeIndexBuf.size, gl.GL_UNSIGNED_INT, self.edgeIndexBuf.v)
	gl.glDisableClientState(gl.GL_VERTEX_ARRAY)

	--gl.glLineWidth(1)
end

function Mesh:drawVertexes(triExplodeDist, groupExplodeDist)
	local gl = require 'gl'
	gl.glColor3f(1,1,1)
	gl.glPointSize(3)

	-- TODO shader that does the explode stuff
	gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].pos.s)
	gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
	gl.glDrawArrays(gl.GL_POINTS, 0, self.vtxs.size)
	gl.glDisableClientState(gl.GL_VERTEX_ARRAY)

	gl.glPointSize(1)
end

function Mesh:drawVertexNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i]
		gl.glVertex3f(v.pos:unpack())
		gl.glVertex3f((v.pos + v.normal):unpack())
	end
	gl.glEnd()
end

function Mesh:drawTriNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for i,t in ipairs(self.tris) do
		gl.glVertex3fv(t.com.s)
		gl.glVertex3fv((t.com + t.normal).s)
	end
	gl.glEnd()
end

function Mesh:drawTriBasis()
	local gl = require 'gl'
	gl.glLineWidth(3)
	gl.glBegin(gl.GL_LINES)
	for i,t in ipairs(self.tris) do
		if t.basis then
			gl.glColor3f(1,0,0)
			gl.glVertex3f(t.com:unpack())
			gl.glVertex3f((t.com + t.basis[1]):unpack())
			gl.glColor3f(0,1,0)
			gl.glVertex3f(t.com:unpack())
			gl.glVertex3f((t.com + t.basis[2]):unpack())
			gl.glColor3f(0,0,1)
			gl.glVertex3f(t.com:unpack())
			gl.glVertex3f((t.com + t.basis[3]):unpack())
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

return Mesh
