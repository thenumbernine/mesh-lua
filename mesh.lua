local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local math = require 'ext.math'
local range = require 'ext.range'
local timer = require 'ext.timer'
local vector = require 'ffi.cpp.vector'
local vec3f = require 'vec-ffi.vec3f'
local box3f = require 'vec-ffi.box3f'

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

local function triNormal(a,b,c)
	local ab = b - a
	local bc = c - b
	local n = ab:cross(bc)
	local len = n:norm()
	if len < 1e-7 or not math.isfinite(len) then
		return vec3f(0,0,0), len
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

function Mesh:init()
	-- TODO new system:
	self.vtxs = vector'MeshVertex_t'
	self.triIndexBuf = vector'int32_t'

	-- just holds extra info per tri
	self.tris = table() -- triangulation of all faces

	self.groups = table()
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
	assert(#self.tris*3 == self.triIndexBuf.size)
	if not self.bbox then self:calcBBox() end
	-- ok the bbox hyp is 28, the smallest maybe valid dist is .077, and everything smalelr is 1e-6 ...
	-- that's a jump from 1/371 to 1/20,000,000
	-- so what's the smallest ratio I should allow?  maybe 1/1million?
	local bboxCornerDist = (self.bbox.max - self.bbox.min):norm()
	local vtxMergeThreshold = bboxCornerDist * 1e-6
print('vtxMergeThreshold', vtxMergeThreshold)
print('before merge vtx count', self.vtxs.size, 'tri count', self.triIndexBuf.size)
	
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
print('after merge vtx count', self.vtxs.size, 'tri count', self.triIndexBuf.size)
	assert(#self.tris*3 == self.triIndexBuf.size)

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
	local t = self.triIndexBuf.v + i
	return self.vtxs.v[t[0]].pos,
			self.vtxs.v[t[1]].pos,
			self.vtxs.v[t[2]].pos
end

function Mesh:removeEmptyTris()
	for i=self.triIndexBuf.size-3,0,-3 do
		local a,b,c = self:triVtxPos(i)
		local area = triArea(a,b,c)
		if area < 1e-7 then
			self:removeTri(i)
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
		local tp1 = self.triIndexBuf.v + 3 * (i1 - 1)
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
					local tp2 = self.triIndexBuf.v + 3 * (i2 - 1)
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

	for i=1,self.triIndexBuf.size/3 do
		self.tris[i] = self.tris[i] or {
			index = i,
		}
	end
	for i=self.triIndexBuf.size/3+1,#self.tris do
		self.tris[i] = nil
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
		for i=0,self.triIndexBuf.size-1,3 do
			local tp = self.triIndexBuf.v + i
			local a = getIndex(tp[0])
			local b = getIndex(tp[1])
			local c = getIndex(tp[2])
			local t = self.tris[i/3+1]
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
	for j=self.triIndexBuf.size-3,0,-3 do
		local t = self.triIndexBuf.v + j
		for i=0,2 do
			if t[i] == from then t[i] = to end
		end
	end
end

function Mesh:removeDegenerateTriangles()
	for i=self.triIndexBuf.size-3,0,-3 do
		local t = self.triIndexBuf.v + i
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
	assert(#self.tris*3 == self.triIndexBuf.size)
	self.triIndexBuf:erase(self.triIndexBuf.v + i, self.triIndexBuf.v + i + 3)
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
	for j=self.triIndexBuf.size-3,0,-3 do
		local t = self.triIndexBuf.v + j
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
		for i=0,self.triIndexBuf.size-1 do
			usedVs[self.triIndexBuf.v[i]] = true
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
		return 0, self.triIndexBuf.size/3-1
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
		local a, b, c = self:triVtxPos(3*i)
		local com = (a + b + c) * (1/3)
		local area = triArea(a, b, c)
		totalCOM = totalCOM + com * area
		totalArea = totalArea + area
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
		local a, b, c = self:triVtxPos(3*i)

		-- using [a,b,c,0] as the 4 pts of our tetrahedron
		-- volume = *<Q,Q> = *(Q∧*Q) where Q = (a-0) ∧ (b-0) ∧ (c-0)
		-- for 3D, volume = det|a b c|
		local com = (a + b + c) * (1/4)

		local volume = tetradVolume(a,b,c)
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
	for i=0,self.triIndexBuf.size-1,3 do
		totalVolume = totalVolume + tetradVolume(self:triVtxPos(i))
	end
	if totalVolume < 0 then totalVolume = -totalVolume end
	return totalVolume
end

function Mesh:clearNormals()
	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].normal:set(0,0,0)
	end
end

function Mesh:breakTriangles()
	print('before breakTriangles, #vtxs '..self.vtxs.size..' #triindexes '..self.triIndexBuf.size)
	local nvtxs = vector('MeshVertex_t', self.triIndexBuf.size)
	local ntris = vector('uint32_t', self.triIndexBuf.size)
	for i=0,self.triIndexBuf.size-1 do
		nvtxs.v[i] = self.vtxs.v[self.triIndexBuf.v[i]]
		ntris.v[i] = i
	end
	self.vtxs = nvtxs
	self.triIndexBuf = ntris
	print('after breakTriangles, #vtxs '..self.vtxs.size..' #triindexes '..self.triIndexBuf.size)

	-- TODO update the mesh ranges as well
	-- assert they do not overlap before
	-- then sort them
	-- then remap them as we break tris

	for i,t in ipairs(self.tris) do
		-- update vertex COMs
		-- they are only valid now
		local com = self.triCOM(self:triVtxPos(3*(i-1)))
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

-- regenerate the vertex normals based on the face normals, weighted average by face area
function Mesh:regenNormals()
	-- calculate vertex normals
	-- TODO store this?  in its own self.vn2s[] or something?
--print('zeroing vertex normals')
	local vtxnormals = vector('vec3f_t', self.vtxs.size)
--print('accumulating triangle normals into vertex normals')
	for i=0,self.triIndexBuf.size-1,3 do
		local ia = self.triIndexBuf.v[i]
		local ib = self.triIndexBuf.v[i+1]
		local ic = self.triIndexBuf.v[i+2]
		-- not sure what i'm doing with these ...
		-- cache or regen?
		local a = self.vtxs.v[ia].pos
		local b = self.vtxs.v[ib].pos
		local c = self.vtxs.v[ic].pos
		local normal, area = triNormal(a,b,c)
		local normalArea = normal * area
		vtxnormals.v[ia] = vtxnormals.v[ia] + normalArea
		vtxnormals.v[ib] = vtxnormals.v[ib] + normalArea
		vtxnormals.v[ic] = vtxnormals.v[ic] + normalArea
	end
--print('normals vertex normals')
	for i=0,vtxnormals.size-1 do
		if vtxnormals.v[i]:norm() > 1e-7 then
			vtxnormals.v[i] = vtxnormals.v[i]:normalize()
		end
--print(k, vtxnormals[i])
	end

	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].normal = vtxnormals.v[i]
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
			gl.glDrawElements(gl.GL_TRIANGLES, g.triCount * 3, gl.GL_UNSIGNED_INT, self.triIndexBuf.v + g.triFirstIndex * 3)
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
	for i=0,self.triIndexBuf.size-1,3 do
		local a, b, c = self:triVtxPos(i)
		local normal = triNormal(a,b,c)
		local com = triCOM(a,b,c)
		gl.glVertex3fv(com.s)
		gl.glVertex3fv((com + normal).s)
	end
	gl.glEnd()
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
	for i=0,self.triIndexBuf.size-3,3 do
		local a,b,c = self:triVtxPos(i)

		local planePt = a
		local triNormal, area = triNormal(a,b,c)
		if area > 1e-7 then
			-- make sure it's pointing towards the ray origin
			if triNormal:dot(dir) < 0 then triNormal = -triNormal end

			local p, s = rayPlaneIntersect(pos, dir, triNormal, planePt)
			if s >= 0 and (not bestdist or s < bestdist) then

				-- barycentric coordinates
				local oob
				local vs = {a,b,c}
				for j=0,2 do
					local v1 = vs[j+1]
					local v2 = vs[(j+1)%3+1]
					local tocom = (v2 - v1):cross(triNormal)
					-- will only work on front-facing triangles
					if (p - v1):dot(tocom) < 0 then
						oob = true
						break
					end
				end

				if not oob then
					besti = i
					bestdist = s
				end
			end
		end
	end
	return besti, bestdist
end

return Mesh
