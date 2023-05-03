local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local math = require 'ext.math'
local timer = require 'ext.timer'
local quat = require 'vec.quat'
local matrix = require 'matrix'
local vector = require 'ffi.cpp.vector'
local vec3f = require 'vec-ffi.vec3f'

ffi.cdef[[
typedef struct {
	vec3f_t pos;
	vec3f_t normal;		//loaded normal
	vec3f_t normal2;	//generated normal ... because i want the viewer to toggle between the two
	vec3f_t texCoord;

	// per-triangle stats (duplicated 3x per-vertex)
	float area;
	vec3f_t com;		//com of tri containing this vertex.  only good for un-indexed drawing.
} obj_vertex_t;
]]


local Triangle = class()

-- a,b,c are face index structures (with .v .vt .vn indexes into mesh.vs .vts .vns)
function Triangle:init(a,b,c)
	self[1] = table(a):setmetatable(nil)
	self[2] = table(b):setmetatable(nil)
	self[3] = table(c):setmetatable(nil)
end

local function triArea(a,b,c)
	local ab = b - a
	local ac = c - a
	local n = ab:cross(ac)
	return .5 * n:norm()
end

-- the 4th pt in the tetrad is zero.  adjust a,b,c accordingly
local function tetradVolume(a,b,c)
	return (a[1] * b[2] * c[3]
		+ a[2] * b[3] * c[1]
		+ a[3] * b[1] * c[2]
		- c[1] * b[2] * a[3]
		- c[2] * b[3] * a[1]
		- c[3] * b[1] * a[2]) / 6
end

-- calculate .normal and .area
function Triangle:calcAux(mesh)
	local a = matrix(mesh.vs[self[1].v])
	local b = matrix(mesh.vs[self[2].v])
	local c = matrix(mesh.vs[self[3].v])
	self.area = triArea(a, b, c)
	self.com = (a + b + c) / 3
	-- TODO what if the tri is degenerate to a line?
	self.normal = (b - a):cross(c - b)
	if self.normal:norm() < 1e-7 then
		self.normal = matrix{0,0,0}
	else
		self.normal = self.normal:normalize()
		if not math.isfinite(self.normal:normSq()) then
			self.normal = matrix{0,0,0}
		end
	end
end


local Mesh = class()

Mesh.Triangle = Triangle

function Mesh:init(filename)
	-- TODO *only* store tris, 
	-- or maybe store indexes too
	-- then the rest pack/unpack upon obj save/load
	self.vs = table()
	self.vts = table()
	self.vns = table()
	self.tris = table() -- triangulation of all faces
end

function Mesh:prepare()
-- [[ calculate bbox.
-- do this before merging vtxs.
	self:calcBBox()
--]]
-- TODO maybe calc bounding radius? Here or later?  That takes COM, which, for COM2/COM3 takes tris.  COM1 takes edges... should COM1 consider merged edges always?  probably...

	self:calcTriAux()

	-- store all edges of all triangles
	-- ... why?  who uses this?
	-- unwrapUVs used to but now it uses the 'allOverlappingEdges' structure
	-- it's used for visualization
	self:findEdges()

	-- calculate coms ...
	self:calcCOMs()
end

function Mesh:calcBBox()
	local box3 = require 'vec.box3'
	self.bbox = box3(-math.huge)
	for _,v in ipairs(self.vs) do
		self.bbox:stretch(v)
	end
end

function Mesh:mergeMatchingVertexes()
	-- ok the bbox hyp is 28, the smallest maybe valid dist is .077, and everything smalelr is 1e-6 ...
	-- that's a jump from 1/371 to 1/20,000,000
	-- so what's the smallest ratio I should allow?  maybe 1/1million?
	local bboxCornerDist = (self.bbox.max - self.bbox.min):norm()
	local vtxMergeThreshold = bboxCornerDist * 1e-6
print('vtxMergeThreshold', vtxMergeThreshold)
	print('before merge vtx count', #self.vs, 'tri count', #self.tris)
	for i=#self.vs,2,-1 do
		for j=1,i-1 do
			local dist = (self.vs[i] - self.vs[j]):norm()
--print(dist)
			if dist < vtxMergeThreshold then
--print('merging vtxs '..i..' and '..j)
				self:mergeVertex(i,j)
				break
			end
		end
	end
	print('after merge vtx count', #self.vs, 'tri count', #self.tris)
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
	local function addEdge(i1, i2, j1, j2, dist, s11, s12, s21, s22, rayPos, rayDir)
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
--for _,t in ipairs(self.tris) do
--	print('n = '..t.normal)
--end
	for i1=#self.tris,2,-1 do
		local t1 = self.tris[i1]
		for j1=1,3 do
			-- t1's j1'th edge
			local v11 = self.vs[t1[j1].v]
			local v12 = self.vs[t1[j1%3+1].v]
--print('tri', i1, 'pos', j1, '=', v11)
			local n1 = v12 - v11
			local n1NormSq = n1:normSq()
			if n1NormSq  > 1e-3 then
				n1 = n1 / math.sqrt(n1NormSq)
				for i2=i1-1,1,-1 do
					local t2 = self.tris[i2]
					for j2=1,3 do
						local v21 = self.vs[t2[j2].v]
						local v22 = self.vs[t2[j2%3+1].v]
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
										addEdge(i1, i2, j1, j2, dist, s11, s12, s21, s22, rayPos, rayDir)
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

function Mesh:calcTriAux()
	-- store tri area
	for _,t in ipairs(self.tris) do
		t:calcAux(self)
	end
end

function Mesh:findEdges()
	-- and just for kicks, track all edges
	timer('edges', function()
		self.edges = {}
		local function addEdge(a,b,t)
			if a > b then return addEdge(b,a,t) end
			self.edges[a] = self.edges[a] or {}
			self.edges[a][b] = self.edges[a][b] or {
				[1] = a,
				[2] = b,
				tris = table(),
				length = (self.vs[a] - self.vs[b]):norm(),
			}
			local e = self.edges[a][b]
			e.tris:insert(t)
			t.edges:insert(e)
		end
		for _,t in ipairs(self.tris) do
			t.edges = table()
			local a,b,c = table.unpack(t)
			addEdge(a.v, b.v, t)
			addEdge(a.v, c.v, t)
			addEdge(b.v, c.v, t)
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
	-- ig i could with edges and vtxs too if I flag them per-material
	timer('mtl com2/3', function()
		for mtlname,mtl in pairs(self.mtllib) do
			mtl.com2 = self:calcCOM2(mtlname)
			mtl.com3 = self:calcCOM3(mtlname)
		end
	end)
end

-- replace all instances of one vertex index with another
function Mesh:replaceVertex(from,to)
--print('replacing vertex ' ..from..' with '..to)
	assert(from > to)
	assert(from >= 1 and from <= #self.vs)
	assert(to >= 1 and to <= #self.vs)
	-- replace in .tris
	for _,t in ipairs(self.tris) do
		for i=1,3 do
			if t[i].v == from then t[i].v = to end
		end
	end
end

function Mesh:removeDegenerateTriangles()
	for i=#self.tris,1,-1 do
		local t = self.tris[i]
		for j=3,2,-1 do
			if t[j].v == t[j-1].v then
				table.remove(t,j)
				break
			end
		end
		if #t < 3 then
--print('removing degenerate tri '..i..' with duplicate vertices')
			self:removeTri(i)
		end
	end
end

function Mesh:removeTri(i)
	self.tris:remove(i)
	for mtlname,mtl in pairs(self.mtllib) do
		if i < mtl.triFirstIndex then
			mtl.triFirstIndex = mtl.triFirstIndex - 1
		elseif i >= mtl.triFirstIndex and i < mtl.triFirstIndex + mtl.triCount then
			mtl.triCount = mtl.triCount - 1
		end
	end
end

-- remove all instances of a veretx index
-- remove the vertex from the elf.vs[] list
-- decrement the indexes greater
function Mesh:removeVertex(vi)
	assert(vi >= 1 and vi <= #self.vs)
	self.vs:remove(vi)
	-- remove in .tris
	-- if you did :replaceVertex and :removeDegenerateFaces first then the rest shouldn't be necessary at all (except for error checking)
	-- if you just straight up remove a vertex then the tris and faces might go out of sync
	for j=#self.tris,1,-1 do
		local t = self.tris[j]
		for i=1,3 do
			if t[i].v == vi then
				--error("found a to-be-removed vertex index in a tri.  you should merge it first, or delete tris containing it first.")
				self:removeTri(j)
				break
			elseif t[i].v > vi then
				t[i].v = t[i].v - 1
			end
		end
	end
end

-- same as removeVertex
-- removes vts[]
-- errors if a tri is still using it
function Mesh:removeTexCoord(vti)
	assert(vti >= 1 and vti <= #self.vts)
	self.vts:remove(vti)
	for j=#self.tris,1,-1 do
		local t = self.tris[j]
		for i=1,3 do
			if t[i].vt == vti then
				error("found a to-be-removed texcoord index in a tri")
			elseif t[i].vt > vti then
				t[i].vt = t[i].vt - 1
			end
		end
	end
end

-- same as removeTexCoord
function Mesh:removeNormal(vni)
	assert(vni >= 1 and vni <= #self.vns)
	self.vns:remove(vni)
	for j=#self.tris,1,-1 do
		local t = self.tris[j]
		for i=1,3 do
			if t[i].vn == vni then
				error("found a to-be-removed normal index in a tri")
			elseif t[i].vn > vni then
				t[i].vn = t[i].vn - 1
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
	local usedVts = {}
	local usedVns = {}
	timer('finding used vertexes', function()
		for _,t in ipairs(self.tris) do
			for j=1,3 do
				usedVs[t[j].v] = true
				if t[j].vt then usedVts[t[j].vt] = true end
				if t[j].vn then usedVns[t[j].vn] = true end
			end
		end
	end)

	timer('removing unused vtxs', function()
		print('before removing, #vs', #self.vs)
		for i=#self.vs,1,-1 do
			if not usedVs[i] then
				self:removeVertex(i)
			end
		end
		print('after removing, #vs', #self.vs)
		print('before removing, #vts', #self.vts)
		for i=#self.vts,1,-1 do
			if not usedVts[i] then
				self:removeTexCoord(i)
			end
		end
		print('after removing, #vts', #self.vts)
		print('before removing, #vns', #self.vns)
		for i=#self.vns,1,-1 do
			if not usedVns[i] then
				self:removeNormal(i)
			end
		end
		print('after removing, #vns', #self.vns)
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

function Mesh:getTriIndexesForMaterial(mtlname)
	if mtlname then
		local mtl = self.mtllib[mtlname]
		if mtl then
			return mtl.triFirstIndex, mtl.triFirstIndex + mtl.triCount - 1
		else
			return 1, 0
		end
	else
		return 1, #self.tris
	end
end

-- yields with each material collection for a particular material name
-- default = no name = iterates over all materials
function Mesh:mtliter(mtlname)
	return coroutine.wrap(function()
		if mtlname then
			local mtl = self.mtllib[mtlname]
			if mtl then coroutine.yield(mtl, mtlname) end
		else
			for mtlname, mtl in pairs(self.mtllib) do
				coroutine.yield(mtl, mtlname)
			end
		end
	end)
end

-- yields with the triangle
-- triangles have [1][2][3] as vi objects which has  .v .vt .vn as indexes into .vs[] .vts[] .vns[]
function Mesh:triiter(mtlname)
	return coroutine.wrap(function()
		for mtl, mtlname in self:mtliter(mtlname) do
			for i=mtl.triFirstIndex,mtl.triFirstIndex+mtl.triCount-1 do
				coroutine.yield(self.tris[i], i)	-- should all pairs/ipairs yield the value first? my table.map does it.  javascript forEach does it.  hmm...
			end
		end
	end)
end

-- same as above, but then yield for each vi individually
function Mesh:triindexiter(mtlname)
	return coroutine.wrap(function()
		for t in self:triiter(mtlname) do
			for i=1,3 do
				coroutine.yield(t[i])
			end
		end
	end)
end

-- calculate COM by 0-forms (vertexes)
function Mesh:calcCOM0()
	local result = self.vs:sum() / #self.vs
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculate COM by 1-forms (edges)
-- depend on self.edges being stored
function Mesh:calcCOM1()
	local totalCOM = matrix{0,0,0}
	local totalLen = 0
	for a,bs in pairs(self.edges) do
		for b in pairs(bs) do
			local v1 = self.vs[a]
			local v2 = self.vs[b]
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
function Mesh:calcCOM2(mtlname)
	local totalCOM = matrix{0,0,0}
	local totalArea = 0
	local i1, i2 = self:getTriIndexesForMaterial(mtlname)
	for i=i1,i2 do
		local t = self.tris[i]
		totalCOM = totalCOM + t.com * t.area
		totalArea = totalArea + t.area
	end
	if totalArea == 0 then
		return self:calcCOM1(mtlname)
	end
	local result = totalCOM / totalArea
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculate COM by 3-forms (enclosed volume)
function Mesh:calcCOM3(mtlname)
	local totalCOM = matrix{0,0,0}
	local totalVolume = 0
	local i1, i2 = self:getTriIndexesForMaterial(mtlname)
	for i=i1,i2 do
		local t = self.tris[i]
		local a = self.vs[t[1].v]
		local b = self.vs[t[2].v]
		local c = self.vs[t[3].v]

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
		return self:calcCOM2(mtlname)
	end
	local result = totalCOM / totalVolume
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculates volume bounded by triangles
function Mesh:calcVolume()
	local totalVolume = 0
	for _,t in ipairs(self.tris) do
		local i,j,k = table.unpack(t)
		-- volume of parallelogram with vertices at 0, a, b, c
		local a = self.vs[i.v]
		local b = self.vs[j.v]
		local c = self.vs[k.v]
		totalVolume = totalVolume + tetradVolume(a,b,c)
	end
	if totalVolume < 0 then totalVolume = -totalVolume end
	return totalVolume
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
	for mtlname, mtl in pairs(self.mtllib) do
		if mtl.image_Kd then
			mtl.tex_Kd = GLTex2D{
				image = mtl.image_Kd,
				minFilter = gl.GL_NEAREST,
				magFilter = gl.GL_LINEAR,
			}
		end
	end

	-- calculate vertex normals
	-- TODO store this?  in its own self.vn2s[] or something?
--print('zeroing vertex normals')
	local vtxnormals = self.vs:mapi(function(v)
		return matrix{0,0,0}
	end)
--print('accumulating triangle normals into vertex normals')
	for t in self:triiter(mtlname) do
		if math.isfinite(t.normal:normSq()) then
			for _,vi in ipairs(t) do
				vtxnormals[vi.v] = vtxnormals[vi.v] + t.normal * t.area
			end
		end
	end
--print('normals vertex normals')
	for k=1,#vtxnormals do
		if vtxnormals[k]:normSq() > 1e-3 then
			vtxnormals[k] = vtxnormals[k]:normalize()
		end
--print(k, vtxnormals[k])
	end

	-- mtl will just index into this.
	-- why does mtl store a list of tri indexes?  it should just store an offset
--print('allocating cpu buffer of obj_vertex_t of size', #self.tris * 3)
	local vtxCPUBuf = vector('obj_vertex_t', #self.tris * 3)
	self.vtxCPUBuf = vtxCPUBuf

	for i,t in ipairs(self.tris) do
		for j,vi in ipairs(t) do
			local dst = vtxCPUBuf.v + (j-1) + 3 * (i-1)
			dst.pos:set(self.vs[vi.v]:unpack())
			if vi.vt then
				if vi.vt < 1 or vi.vt > #self.vts then
					print("found an oob vt "..vi.vt)
					dst.texCoord:set(0,0,0)
				else
					dst.texCoord:set(self.vts[vi.vt]:unpack())
				end
			end
			if vi.vn then
				if vi.vn < 1 or vi.vn > #self.vns then
					print("found an oob fn "..vi.vn)
					dst.normal:set(0,0,0)
				else
					dst.normal:set(self.vns[vi.vn]:unpack())
				end
			end
			dst.normal2:set(vtxnormals[vi.v]:unpack())
			dst.area = t.area
			dst.com:set(t.com:unpack())
		end
	end

--print('creating array buffer of size', self.vtxCPUBuf.size)
	self.vtxBuf = GLArrayBuffer{
		size = self.vtxCPUBuf.size * ffi.sizeof'obj_vertex_t',
		data = self.vtxCPUBuf.v,
		usage = gl.GL_STATIC_DRAW,
	}
	assert(glreport'here')

	self.vtxAttrs = {}
	for _,info in ipairs{
		{name='pos', size=3},
		{name='texCoord', size=3},
		{name='normal', size=3},
		{name='normal2', size=3},
		{name='com', size=3},
	} do
		local srcAttr = shader.attrs[info.name]
		if srcAttr then
			self.vtxAttrs[info.name] = GLAttribute{
				buffer = self.vtxBuf,
				size = info.size,
				type = gl.GL_FLOAT,
				stride = ffi.sizeof'obj_vertex_t',
				offset = ffi.offsetof('obj_vertex_t', info.name),
			}
			assert(glreport'here')
		end
	end
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

function Mesh:draw(args)
	local gl = require 'gl'

	self:loadGL()	-- load if not loaded

	local curtex
	for mtlname, mtl in pairs(self.mtllib) do
		--[[
		if mtl.Kd then
			gl.glColor4f(mtl.Kd:unpack())
		else
			gl.glColor4f(1,1,1,1)
		end
		--]]
		--[[
		if mtl
		and mtl.tex_Kd
		and not (args and args.disableTextures)
		then
			-- TODO use .Ka, Kd, Ks, Ns, etc
			-- with fixed pipeline?  opengl lighting?
			-- with a shader in the wavefrontobj lib?
			-- with ... nothing?
			curtex = mtl.tex_Kd
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
		if args.beginMtl then args.beginMtl(mtl) end

		--[[ immediate mode
		gl.glBegin(gl.GL_TRIANGLES)
		for vi in self:triindexiter(mtlname) do
			-- TODO store a set of unique face v/vt/vn index-vertexes
			-- and then bake those into a unique vertex array, and store its index alongside face's other indexes
			-- that'll be most compat with GL indexed arrays
			if vi.vt then
				gl.glTexCoord2f(self.vts[vi.vt]:unpack())
			end
			if vi.vn then
				gl.glNormal3f(self.vns[vi.vn]:unpack())
			end
			gl.glVertex3f(self.vs[vi.v]:unpack())
		end
		gl.glEnd()
		--]]
		--[[ vertex client arrays
		gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].pos.s)
		gl.glTexCoordPointer(3, gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].texCoord.s)
		gl.glNormalPointer(gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].normal.s)
		gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
		gl.glEnableClientState(gl.GL_TEXTURE_COORD_ARRAY)
		gl.glEnableClientState(gl.GL_NORMAL_ARRAY)
		gl.glDrawArrays(gl.GL_TRIANGLES, 0, mtl.vtxCPUBuf.size)
		gl.glDisableClientState(gl.GL_VERTEX_ARRAY)
		gl.glDisableClientState(gl.GL_TEXTURE_COORD_ARRAY)
		gl.glDisableClientState(gl.GL_NORMAL_ARRAY)
		--]]
		--[[ vertex attrib pointers ... requires specifically-named attrs in the shader
		gl.glVertexAttribPointer(args.shader.attrs.pos.loc, 3, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].pos.s)
		gl.glVertexAttribPointer(args.shader.attrs.texCoord.loc, 3, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].texCoord.s)
		gl.glVertexAttribPointer(args.shader.attrs.normal.loc, 3, gl.GL_FLOAT, gl.GL_TRUE, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].normal.s)
		gl.glEnableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.texCoord.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.normal.loc)
		gl.glDrawArrays(gl.GL_TRIANGLES, 0, mtl.vtxCPUBuf.size)
		gl.glDisableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.texCoord.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.normal.loc)
		--]]
		-- [[ vao ... getting pretty tightly coupled with the view.lua file ...
		if mtl.triCount > 0 then
			self.vao:use()
			gl.glDrawArrays(gl.GL_TRIANGLES, (mtl.triFirstIndex-1) * 3, mtl.triCount * 3)
			self.vao:useNone()
		end
		--]]
		if args.endMtl then args.endMtl(mtl) end
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
-- can't handle mtl-group explode dist because edges aren't stored associted with materials ...
-- they are per-tri, which is per-face, which is per-material, but there can be multiple materials per edge.
function Mesh:drawEdges(triExplodeDist, groupExplodeDist)
	local gl = require 'gl'
	gl.glLineWidth(3)
	gl.glColor3f(1,1,0)
	gl.glBegin(gl.GL_LINES)
	for a,other in pairs(self.edges) do
		for b,edge in pairs(other) do
			-- avg of explode offsets of all touching tris
			local offset = matrix{0,0,0}
			for _,t in ipairs(edge.tris) do
				-- get mtl for tri, then do groupExplodeDist too
				-- matches the shader in view.lua
				local groupExplodeOffset = (t.mtl.com3 - self.com3) * groupExplodeDist
				local triExplodeOffset = (t.com - t.mtl.com3) * triExplodeDist
				offset = offset + groupExplodeOffset + triExplodeOffset
			end
			offset = offset / #edge.tris
			gl.glVertex3f((self.vs[a] + offset):unpack())
			gl.glVertex3f((self.vs[b] + offset):unpack())
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

function Mesh:drawVertexes(triExplodeDist, groupExplodeDist)
	local gl = require 'gl'
	gl.glColor3f(1,1,1)
	gl.glPointSize(3)
	gl.glBegin(gl.GL_POINTS)
	for i,v in ipairs(self.vs) do
		-- avg of explode offsets of all touching tris
		local offset = matrix{0,0,0}
		-- get mtl for tri, then do groupExplodeDist too
		-- matches the shader in view.lua
		local triExplodeOffset = (v - self.com3) * triExplodeDist
		offset = offset + triExplodeOffset
		gl.glVertex3f((v + offset):unpack())
	end
	gl.glEnd()
	gl.glPointSize(1)
end



function Mesh:drawStoredNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for _,t in ipairs(self.tris) do
		for _,vi in ipairs(t) do
			if vi.vn then
				local v = self.vs[vi.v]
				local vn = self.vns[vi.vn]
				gl.glVertex3f(t.com:unpack())
				gl.glVertex3f((t.com + vn):unpack())
				--gl.glVertex3f((v.com + v.normal2):unpack())
			end
		end
	end
	gl.glEnd()
end

function Mesh:drawVertexNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for mtlname,mtl in pairs(self.mtllib) do
		if mtl.vtxCPUBuf then
			for i=0,mtl.vtxCPUBuf.size-1,3 do
				local v = mtl.vtxCPUBuf.v[i]
				gl.glVertex3f(v.pos:unpack())
				gl.glVertex3f((v.pos + v.normal2):unpack())
			end
		end
	end
	gl.glEnd()
end

function Mesh:drawTriNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for _,t in ipairs(self.tris) do
		gl.glVertex3f(t.com:unpack())
		gl.glVertex3f((t.com + t.normal):unpack())
	end
	gl.glEnd()
end

function Mesh:findClosestVertexToMouseRay(pos, dir, fwd, cosEpsAngle)
	-- assumes dir is 1 unit fwd along the view fwd
	--dir = dir:normalize()
	local dirlen = dir:norm()
	local bestdot, besti, bestdepth
	for i,v in ipairs(self.vs) do
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

return Mesh
