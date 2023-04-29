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
	if self.normal:normSq() < 1e-3 then
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
	self.vs = table()
	self.vts = table()
	self.vns = table()
	self.tris = table() -- triangulation of all faces
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
function Mesh:calcAllOverlappingEdges()
	--[[
	these are whatever mesh edges are partially overlapping one another.
	they are a result of a shitty artist.
	because of which, there is no guarantee with this table that each tri has 3 edges, and each edge has only 2 tris.
	instead it's a shitfest shitstorm.
	--]]
	self.allOverlappingEdges = {}
	for _,t in ipairs(self.tris) do
		t.allOverlappingEdges = table()
	end
	local function addEdge(i1, i2, j1, j2, dist, s11, s12, s21, s22, planeOrigin, planeNormal)
		-- in my loop i2 < i1, but i want it ordered lowest-first, so ... swap them
		assert(i2 < i1)
		self.allOverlappingEdges[i2] = self.allOverlappingEdges[i2] or {}
		self.allOverlappingEdges[i2][i1] = self.allOverlappingEdges[i2][i1] or {
			[1] = i2,
			[2] = i1,
			triVtxIndexes = {j2, j1},
			intervals = {{s21,s22}, {s11,s12}},
			tris = table(),
			dist = dist,
			planeOrigin = planeOrigin,
			planeNormal = planeNormal
		}
		local e = self.allOverlappingEdges[i2][i1]
		local t1 = self.tris[i1]
		local t2 = self.tris[i2]
		e.tris:insertUnique(t2)
		e.tris:insertUnique(t1)
		t1.allOverlappingEdges:insertUnique(e)
		t2.allOverlappingEdges:insertUnique(e)
	end
	for i1=#self.tris,2,-1 do
		local t1 = self.tris[i1]
		for j1=1,3 do
			-- t1's j1'th edge
			local v11 = self.vs[t1[j1].v]
			local v12 = self.vs[t1[j1%3+1].v]
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
								-- normals align, calculate distance
								local planeOrigin = v11	-- pick any point on line v1: v11 or v12
								local planeNormal = n1
								local dv = v21 - planeOrigin	-- ray from the v1 line to any line on v2
								dv = dv - planeNormal * dv:dot(planeNormal)		-- project onto the plane normal
								local dist = dv:norm()
								if dist < 1e-3 then

									-- now find where along plane normal the intervals {v11,v12} and {v21,v22}
									local s11 = 0	--(v11 - planeOrigin):dot(planeNormal) -- assuming v11 is the plane origin
									local s12 = (v12 - planeOrigin):dot(planeNormal)
									-- based on n1 being the plane normal, s11 and s12 are already sorted
									local s21 = (v21 - planeOrigin):dot(planeNormal)
									local s22 = (v22 - planeOrigin):dot(planeNormal)
									-- since these aren't, they have to be sorted
									if s21 > s22 then s21, s22 = s22, s21 end
									if s11 < s22 and s12 > s21 then
										addEdge(i1, i2, j1, j2, dist, s11, s12, s21, s22, planeOrigin, planeNormal)
									end
								end
							end
						end
					end
				end
			end
		end
	end
	for a,o in pairs(self.allOverlappingEdges) do
		for b,e in pairs(o) do
			print(
				'edges', e[1], e.triVtxIndexes[1],
				'and', e[2], e.triVtxIndexes[2],
				'align with dist', e.dist,
				'with projected intervals', table.concat(e.intervals[1], ', '),
				'and', table.concat(e.intervals[2], ', '))
		end
	end
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
			assert(not t.edges)
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

--[[
1) replace the 'from' with the 'to'
2) remove any degenerate triangles/faces
3) remove the to vertex from the list
--]]
function Mesh:mergeVertex(from,to)
	assert(from > to)
	self:replaceVertex(from,to)
	self:removeDegenerateTriangles()
	self:removeVertex(from)
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
print('allocating cpu buffer of obj_vertex_t of size', #self.tris * 3)
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

print('creating array buffer of size', self.vtxCPUBuf.size)
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

function Mesh:drawUVUnwrapEdges(_3D)
	local gl = require 'gl'
	local eps = 1e-3
	-- [[ show unwrap info
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for _,info in ipairs(self.unwrapUVEdges or {}) do
		for i,t in ipairs(info) do
			if info.floodFill == true then
				gl.glColor3f(0,0,1)
			else
				if i==1 then
					gl.glColor3f(0,1,0)
				else
					gl.glColor3f(1,0,0)
				end
			end
			gl.glVertex3f((t.com + eps * t.normal):unpack())
		end
	end
	gl.glEnd()
	gl.glPointSize(3)
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_POINTS)
	for _,v in ipairs(self.unwrapUVOrigins or {}) do
		gl.glVertex3f(v:unpack())
	end
	gl.glEnd()
	gl.glPointSize(1)
	--]]
end


-- this belongs in its own place, outside this project


function Mesh:unwrapUVs()
-- TODO put this all in its own function or its own app
	local numSharpEdges = 0
	for a,other in pairs(self.allOverlappingEdges) do
		for b,edge in pairs(other) do
			-- #tris == 0 is an edge construction error
			-- #tris == 1 is a sharp edge ... which means a non-convex
			-- #tris == 2 is good
			-- any more ... we have something weird
			if #edge.tris == 0 then
				error'here'
			elseif #edge.tris == 1 then
				numSharpEdges = numSharpEdges + 1
			elseif #edge.tris > 2 then
				print('found an edge with != 2 tris: ' ..#edge.tris)
			end
		end
	end
	print('numSharpEdges = '..numSharpEdges)

	-- how about count area per cube sides?
	-- total vector, l=0 s.h.
	local avgNormal = matrix{0,0,0}
	for _,t in ipairs(self.tris) do
		avgNormal = avgNormal + t.normal * t.area
	end
	local avgNormalIsZero = avgNormal:normSq() < 1e-7
	if not avgNormalIsZero then avgNormal = avgNormal:normalize() end
	print('avg normal = '..avgNormal)

	-- the same idea as the l=1 spherical harmonics
	local range = require 'ext.range'
	local areas = matrix{6}:zeros()
	for _,t in ipairs(self.tris) do
		local _,i = table.sup(t.normal:map(math.abs))
		assert(i)
		local dir = t.normal[i] > 0 and 1 or 2
		local index = dir + 2 * (i-1)
		areas[index] = areas[index] + t.area
	end
	print('per-side x plus/minus normal distribution = '..require 'ext.tolua'(areas))

	local bestNormal
-- TODO snap-to-axis for within epsilon
--	if not avgNormalIsZero then
--		bestNormal = matrix(avgNormal)
do--	else
		local _, besti = table.sup(areas)
		local bestdir = math.floor((besti-1)/2)+1
		bestNormal = matrix{0,0,0}
		bestNormal[bestdir] = bestdir%2 == 0 and -1 or 1
	end
	print('bestNormal', bestNormal)

	-- for all faces (not checked)
	--  traverse neighbors by edge and make sure the normals align
	--  complain if the normals flip
	--  or should this be robust enough to determine volume without correct normals / tri order?
	--  I'll assume ccw polys for now.
	local function findLocalIndex(t, v)
		for i=1,3 do
			if t[i].v == v then return i end
		end
	end
	local function getEdgeOppositeTri(e, t)
		assert(#e.tris == 2)
		local t1,t2 = table.unpack(e.tris)
		if t2 == t then
			t1, t2 = t2, t1
		end
		assert(t1 == t)
		return t2, t1
	end
	local function calcUVBasis(t, tsrc, esrc)
		assert(not t[1].uv and not t[2].uv and not t[3].uv)
		-- t[1] is our origin
		-- t[1]->t[2] is our x axis with unit length
		local v = matrix{3,3}:lambda(function(i,j) return self.vs[t[i].v][j] end)
--print('v\n'..v)
		local d1 = v[2] - v[1]
		local d2 = v[3] - v[2]
		local n = d1:cross(d2)
		local nlen = n:norm()
--print('|d1 x d2| = '..nlen)
		if not math.isfinite(nlen)
		or nlen < 1e-9
		then
			t.normal = d1:normalize()
			-- can't fold this because i'ts not a triangle ... it's a line
			-- should I even populate the uv fields?  nah, just toss it in the caller
			return true
		end
		n = n / nlen
--print('n = '..n)
		t.normal = matrix(n)

		--if true then
		if not tsrc then	-- first basis
			t.uvorigin2D = matrix{0,0}
			-- modularity for choosing which point on the tri is the uv origin
			--[[ use the first point
			t.uvorigin3D = matrix(v[1])
			--]]
			-- [[ use the y-lowest point
			-- good for roofs ... not for walls ...
			t.uvorigin3D = matrix(v[
				select(2, range(3):mapi(function(i)
					return v[i][2]
				end):inf())
			])
			self.unwrapUVOrigins:insert(t.uvorigin3D * .7 + t.com * .3)
			--]]

--print('uv2D = '..t.uvorigin2D)
--print('uv3D = '..t.uvorigin3D)

			-- modularity for choosing initial basis
			--[[ use first base of the triangle
			local ex = d1:normalize()
			--]]
			--[[ preference to align the first axis in the xz plane
			-- first find the best option of the 3 deltas
			-- close to the same as choosing n cross y+
			-- but the first set of tris are not so good
			local d3 = v[1] - v[3]
			local ex
			if math.abs(d1[2]) < math.abs(d2[2]) then	-- d1 < d2
				if math.abs(d1[2]) < math.abs(d3[2]) then	-- d1 < d2 and d1 < d3
					ex = d1:normalize()
				else			-- d3 < d1 < d2
					ex = d3:normalize()
				end
			else	-- d2 < d1
				if math.abs(d2[2]) < math.abs(d3[2]) then	-- d2 < d1 and d2 < d3
					ex = d2:normalize()
				else			-- d3 < d2 < d1
					ex = d3:normalize()
				end
			end
			--]]
			--[[ instead of choosing lowest delta in xz plane ...
			-- first prioritize dy=0
			-- then prioritize dx=0 or dz = 0
			local d3 = v[1] - v[3]
			local ex = table{d1:normalize(),d2:normalize(),d3:normalize()}:sort(function(a,b)
				if not math.isfinite(a:normSq()) then return false end
				if not math.isfinite(b:normSq()) then return true end
				return range(3):mapi(function(i) return a[i] == 0 and 1 or 0 end):sum()
					> range(3):mapi(function(i) return b[i] == 0 and 1 or 0 end):sum()
			end)[1]:normalize()
			print('ex', ex)
			--]]
			--[[ just use n cross y+
			-- BEST FOR CARTESIAN ALIGNED
			-- best for top
			-- crashes for sides
			local ex = n:cross(bestNormal):normalize()
			--]]
			--[[ just use n cross x+ or z+ ...
			-- ... gets nans
			local ex = n:cross{0,0,1}:normalize()
			--]]
			--[[ pick whatever is most perpendicular to n and another cartesian basis
			-- a[i] = 1, i = sup(|n[i]|) gives same as n cross y+, good for tops, but crashes for sides.
			-- a[i] = 1, i = inf(|n[i]|) doesn't crash for sides but gives bad tops results.
			-- a[i+1] = 1, i = inf(|n[i]|) crashes on sides, but same as n cross y+ on top
			local _, i = table.sup(n:map(math.abs))
			local a = matrix{0,0,0}
			a[i] = 1
			local ex = n:cross(a):normalize()
			assert(math.isfinite(ex:normSq()))
			--]]
			--[[ draw a line between the lowest two points
			if v[1][2] > v[2][2] then
				if v[1][2] > v[3][2] then	-- 1 highest
					ex = (v[3] - v[2]):normalize()
				else	-- 3 highest
					ex = (v[2] - v[1]):normalize()
				end
			else
				if v[2][2] > v[3][2] then	-- 2 highest
					ex = (v[1] - v[3]):normalize()
				else	-- 3 highest
					ex = (v[2] - v[1]):normalize()
				end
			end
			--]]
			-- [[ most orthogonal to bestNormal take 2
			local d3 = v[1] - v[3]
			local ds = table{d1:normalize(), d2:normalize(), d3:normalize()}
			local dots = ds:mapi(function(d) return math.abs(d:dot(bestNormal)) end)
			local i = range(3):sort(function(a,b) return dots[a] < dots[b] end)[1]
			local ex = ds[i]
			ex = n:cross(ex):normalize()
			--]]

			-- fallback, if n is nan or zero
			local exNormSq = ex:normSq()
			if exNormSq < 1e-3						-- can't use zero
			or not math.isfinite(exNormSq)			-- can't use nan
			or math.abs(ex:dot(n)) > 1 - 1e-3	-- can't use ex perp to n
			then
print('failed to find u vector based on bestNormal, picked ex='..ex..' from bestNormal '..bestNormal)
				-- pick any basis perpendicular to 'n'
				local ns = matrix{3}:lambda(function(i)
					local a = matrix{0,0,0}
					a[i] = 1
					return n:cross(a)
				end)
--print('choices\n'..ns)
				local lens = matrix{3}:lambda(function(i) return ns[i]:normSq() end)
				local _, i = table.sup(lens)	-- best normal
--print('biggest cross '..i)
				ex = ns[i]:normalize()
print('picking fallback ', ex)
			end

--print('ex = '..ex)
			-- tangent space.  store as row vectors i.e. transpose, hence the T
			t.uvbasisT = matrix{
				ex,
				n:cross(ex):normalize(),
				n,
			}
--print('ey = '..t.uvbasisT[2])
		else
			assert(tsrc[1].uv and tsrc[2].uv and tsrc[3].uv)

--[[
tsrc.v3      tsrc.v2
	   *-------* t.v2
	   |   ___/|
	   |__/    |
tsrc.v1*-------*
	  t.v3   t.v1
--]]
--print('folding from', tsrc.index, 'to', t.index)
			--[[ using .edges
			local i11 = findLocalIndex(tsrc, esrc[1])	-- where in tsrc is the edge's first?
			local i12 = findLocalIndex(tsrc, esrc[2])	-- where in tsrc is the edge's second?
			local i21 = findLocalIndex(t, esrc[1])	-- where in t is the edge's first?
			local i22 = findLocalIndex(t, esrc[2])	-- where in t is the edge's second?
			assert(i11 and i12 and i21 and i22)
			assert(tsrc[i11].v == t[i21].v)	-- esrc[1] matches between tsrc and t
			assert(tsrc[i12].v == t[i22].v)	-- esrc[2] matches between tsrc and t
			assert(tsrc[i11].v == esrc[1])
			assert(tsrc[i12].v == esrc[2])
			assert(t[i21].v == esrc[1])
			assert(t[i22].v == esrc[2])
			--]]
			-- [[ using .allOverlappingEdges
			local i11 = esrc.triVtxIndexes[1]
			local i12 = i11 % 3 + 1
			local i21 = esrc.triVtxIndexes[2]
			local i22 = i21 % 3 + 1
			assert(i11 and i12 and i21 and i22)
			--]]
--print('edge local vtx indexes: tsrc', i11, i12, 't', i21, i22)
			-- tables are identical

			local isrc
			if tsrc[i11].uv then
				isrc = i11
			elseif tsrc[i12].uv then
				isrc = i12
			else
				error("how can we fold a line when the src tri doesn't have uv coords for it?")
			end
			t.uvorigin2D = matrix(tsrc[isrc].uv)			-- copy matching uv from edge neighbor
			t.uvorigin3D = matrix(self.vs[tsrc[isrc].v])	-- copy matching 3D position
--print('uv2D = '..t.uvorigin2D)
--print('uv3D = '..t.uvorigin3D)

			-- modularity for choosing unwrap rotation
			--[[ reset basis every time. dumb.
			local ex = d1:normalize()
			t.uvbasisT = matrix{
				ex,
				n:cross(ex):normalize(),
				n,
			}
			--]]
			--[[ subsequent tri basis should be constructed from rotating the prev tri basis
			-- find the rotation from normal 1 to normal 2
			-- that'll just be the matrix formed from n1 and n2's basis ...
			local q = quat():vectorRotate(tsrc.normal, t.normal)
			t.uvbasisT = matrix{
				q:rotate(tsrc.uvbasisT[1]),
				q:rotate(tsrc.uvbasisT[2]),
				n,
			}
			--]]
			-- [[ pick the rotation along the cardinal axis that has the greatest change
			-- BEST FOR CARTESIAN ALIGNED
			local dn = t.normal - tsrc.normal
			local q
			if dn:normSq() < 1e-3 then
				q = quat(0,0,0,1)
			else
				-- pick smallest changing axis in normal?
				local _, i = table.inf(dn:map(math.abs))
				if i == 1 then
					local degrees = math.deg(math.atan2(n[3], n[2]) - math.atan2(tsrc.normal[3], tsrc.normal[2]))
--print(t.normal, tsrc.normal, dn, 'rot on x-axis by', degrees)
					q = quat():fromAngleAxis(1, 0, 0, degrees)
				elseif i == 2 then
					local degrees = math.deg(math.atan2(n[1], n[3]) - math.atan2(tsrc.normal[1], tsrc.normal[3]))
--print(t.normal, tsrc.normal, dn, 'rot on y-axis by', degrees)
					q = quat():fromAngleAxis(0, 1, 0, degrees)
				elseif i == 3 then
					local degrees = math.deg(math.atan2(n[2], n[1]) - math.atan2(tsrc.normal[2], tsrc.normal[1]))
--print(t.normal, tsrc.normal, dn, 'rot on z-axis by', degrees)
					q = quat():fromAngleAxis(0, 0, 1, degrees)
				end
			end
--print('q', q)
--print('n', n)
--print('tsrc ex = '..tsrc.uvbasisT[1])
--print('tsrc ey = '..tsrc.uvbasisT[2])
			t.uvbasisT = matrix{
				q:rotate(tsrc.uvbasisT[1]),
				q:rotate(tsrc.uvbasisT[2]),
				n,
			}
			--]]

--print('|ez-n| = '..matrix(q:rotate(tsrc.uvbasisT[3]) - n):norm())
--print('ex = '..t.uvbasisT[1])
--print('ey = '..t.uvbasisT[2])
		end

		for i=1,3 do
			local d = v[i] - t.uvorigin3D
			local m = matrix{t.uvbasisT[1], t.uvbasisT[2]}
--print('d = '..d)
--print('m\n'..m)
--print('m * d = '..(m * d))
			t[i].uv = m * d + t.uvorigin2D
--print('uv = '..t[i].uv)
			if not math.isfinite(t[i].uv:normSq()) then
				print('tri has nans in its basis')
			end
		end
	end

	self.unwrapUVOrigins = table()
	self.unwrapUVEdges = table()	-- keep track of how it's made for visualization's sake ...

	local notDoneYet = table(self.tris)
	local done = table()

	local function calcUVBasisAndAddNeighbors(t, tsrc, e, todo)
		if tsrc then self.unwrapUVEdges:insert{tsrc, t} end
		-- calc the basis by rotating it around the edge
		assert((tsrc == nil) == (e == nil))
		local gotBadTri = calcUVBasis(t, tsrc, e)
		-- TODO roof actually looks good with always retarting ... but not best
		if not gotBadTri then
			done:insert(t)
			assert(t[1].uv and t[2].uv and t[3].uv)
			-- insert neighbors into a to-be-calcd list
--print('tri', t.index)
			for _,e in ipairs(t.allOverlappingEdges) do
--print('edge length', e.length)
				-- for all edges in the t, go to the other faces matching.
				-- well, if there's more than 2 faces shared by an edge, that's a first hint something's wrong.
				do--if #e.tris == 2 then	-- if we're using any overlapping edge then this guarantee goes out the window
					local t2 = getEdgeOppositeTri(e, t)
-- if our tri
-- ... isn't in the 'todo' pile either ...
-- ... is still in the notDoneYet pile ...
					if not todo:find(t2)
					and not done:find(t2)
					then
						local i = notDoneYet:find(t2)
						if i then
							assert(not t2[1].uv and not t2[2].uv and not t2[3].uv)
							notDoneYet:remove(i)
							todo:insert(t2)
						end
					end
				end
			end
		end
	end

	local function floodFillMatchingNormalNeighbors(t, tsrc, e, alreadyFilled)
		alreadyFilled:insertUnique(t)
		if t[1].uv then return end
		if tsrc then self.unwrapUVEdges:insert{tsrc, t, floodFill=true} end
		assert((tsrc == nil) == (e == nil))
		if not calcUVBasis(t, tsrc, e) then
			done:insert(t)
			assert(t[1].uv and t[2].uv and t[3].uv)
			for _,e in ipairs(t.allOverlappingEdges) do
				if #e.tris == 2 then
					local t2 = getEdgeOppositeTri(e, t)
					if not alreadyFilled:find(t2) then
						if t.normal:dot(t2.normal) > 1 - 1e-3 then
							floodFillMatchingNormalNeighbors(t2, t, e, alreadyFilled)
						else
							alreadyFilled:insertUnique(t)
						end
					end
				end
			end
		end
	end

	while #notDoneYet > 0 do
--print('starting unwrapping process with '..#notDoneYet..' left')

		-- I will be tracking all live edges
		-- so process the first tri as the starting point
		-- then add its edges into the 'todo' list

		-- modularity heuristic of picking best starting edge
		--[[ take the first one regardless
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[[ largest tri first
		notDoneYet:sort(function(a,b) return a.area > b.area end)
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[[ choose the first edge that starts closest to the ground (lowest y value)
		-- ... but this does some whose sharp edges touches
		-- i really want only those with flat edges at the base
		notDoneYet:sort(function(a,b)
			return math.min(
				self.vs[a[1].v][2],
				self.vs[a[2].v][2],
				self.vs[a[3].v][2]
			) < math.min(
				self.vs[b[1].v][2],
				self.vs[b[2].v][2],
				self.vs[b[3].v][2]
			)
		end)
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[[ same as above but pick the lowest *edge* , not *vtx*, cuz we want the base edges aligned with the bottom
		notDoneYet:sort(function(a,b)
			local aEdgeYMin = matrix{3}:lambda(function(i)
				return .5 * (self.vs[a[i].v][2] + self.vs[a[i%3+1].v][2])
			end):min()
			local bEdgeYMin = matrix{3}:lambda(function(i)
				return .5 * (self.vs[b[i].v][2] + self.vs[b[i%3+1].v][2])
			end):min()
			return aEdgeYMin < bEdgeYMin
		end)
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[=[ choose *all* tris whose flat edges are at the minimum y
		local vtxsNotDoneYet = {}
		for _,t in ipairs(notDoneYet) do
			for i=1,3 do
				vtxsNotDoneYet[t[i].v] = true
			end
		end
		-- convert set of keys to list
		vtxsNotDoneYet = table.keys(vtxsNotDoneYet):sort(function(a,b)
			return self.vs[a][2] < self.vs[b][2]	-- sort by y axis
		end)
		local eps = (self.bbox.max[2] - self.bbox.min[2]) * 1e-5
		local ymin = self.vs[vtxsNotDoneYet[1]][2]
		print('y min', ymin)
		-- now go thru all tris not done yet
		-- if any have 2/3 vtxs at the min then add them
		local todo = table()
		for i=#notDoneYet,1,-1 do
			local t = notDoneYet[i]
			local mincount = 0
			for j=1,3 do
				if self.vs[t[j].v][2] < ymin + eps then mincount = mincount + 1 end
			end
			if mincount >= 2 then
				todo:insert(notDoneYet:remove(i))
			end
		end
		-- if none were added then add one with 1/3 vtxs at the min
		if #todo == 0 then
			for i=#notDoneYet,1,-1 do
				local t = notDoneYet[i]
				for j=1,3 do
					if self.vs[t[j].v][2] < ymin + eps then
						todo:insert(notDoneYet:remove(i))
						break
					end
				end
				if #todo > 0 then break end
			end
		end
print('number to initialize with', #todo)
		-- ... and process them all once first, adding their neigbors to the 'todo' pile
		--]=]
		-- [=[ choose tris with any edges that are 90' from the guide normal
		-- but not if the vector from the com to the edges is towards the guide normal
		local todo = table()
		for i=#notDoneYet,1,-1 do
			local t = notDoneYet[i]
			for j=1,3 do
				local a = self.vs[t[j].v]
				local b = self.vs[t[j%3+1].v]
				if math.abs((b - a):normalize():dot(bestNormal)) < 1e-5 then
					-- exclude tops
					if (.5 * (b + a) - t.com):dot(bestNormal) > 0 then
						notDoneYet:remove(i)
						todo:insert(t)
						break
					end
				end
			end
		end
		-- if finding a y-perpendicular downward-pointing edge was too much to ask,
		-- ... then pick one at random?
		if #todo == 0 then
--print("couldn't find any perp-to-bestNormal edges to initialize with...")
			todo:insert(notDoneYet:remove(1))
		end
		--]=]

		-- [[ first pass to make sure all the first picked are considered
		-- during this first pass, immediately fold across any identical normals
--print('starting first pass with #todo', #todo)
		for i=#todo,1,-1 do
			local t = todo:remove(i)
			-- for 't', flood-fill through anything with matching normal
			-- while flood-filling, continue adding neighbors to 'todo'
			local filled = table()
			floodFillMatchingNormalNeighbors(t, nil, nil, filled)
			for _,t in ipairs(filled) do
				if not t[1].uv then
					todo:insertUnique(t)
				end
			end
		end
--print('after first pass, #todo', #todo, '#done', #done)
		--]]

		while #todo > 0 do
			local t, tsrc, e

			-- pick best edge between any triangle in 'done' and any in 'todo'
			local edgesToCheck = table()
			for _,t in ipairs(todo) do
				for _,e in ipairs(t.allOverlappingEdges) do
					if #e.tris == 2 then
						local t2 = getEdgeOppositeTri(e, t)
						if done:find(t2) then
							edgesToCheck:insert{tri=t, edge=e, prevtri=t2}
						end
					end
				end
			end
			if #edgesToCheck == 0 then
				-- same as first iter
				print("no edges to check ...")
				t = todo:remove(math.random(#todo))
			else
				-- assert from prevoius iteration that the first is the best
				-- modularity heuristic for picking best continuing edge
				-- sort last instead of first, so first iteration and first entry is removed, so I can guarantee that all entries have .prevtri and .edge
				edgesToCheck:sort(function(a,b)
					local ea, eb = a.edge, b.edge
					--[[ prioritize longest edge ... cube makes a long straight shape with one bend.
					-- looks best for cone.  just does two solid pieces for the base and sides
					-- looks best for cube.  does the cubemap t.
					return ea.length > eb.length
					--]]
					--[[ prioritize shortest edge ... cube makes a zigzag
					return ea.length < eb.length
					--]]
					--[[ prioritize biggest area
					local atriarea = a.tri.area + a.prevtri.area
					local btriarea  = b.tri.area + b.prevtri.area
					return atriarea > btriarea
					--]]
					--[[ prioritize smallest area
					local atriarea = a.tri.area + a.prevtri.area
					local btriarea  = b.tri.area + b.prevtri.area
					return atriarea < btriarea
					--]]
					-- [[ prioritize rotation angle
					-- LOOKS THE BEST SO FAR
					local ra = a.tri.normal:dot(a.prevtri.normal)
					local rb = b.tri.normal:dot(b.prevtri.normal)
					return ra > rb
					--]]
					-- TODO Try prioritizing discrete curvature (mesh angle & area info combined?)
					--[[ prioritize by rotation.
					-- first priority is no-rotation
					-- next is predominantly y-axis rotations
					local dn = a.tri.normal - a.prevtri.normal
					local _, i = table.inf(dn:map(math.abs))
					--]]
				end)
				local check = edgesToCheck[1]
				t, e, tsrc = check.tri, check.edge, check.prevtri
				assert(t)
				assert(e)
				assert(tsrc)
				assert(tsrc[1].uv and tsrc[2].uv and tsrc[3].uv)
				todo:removeObject(t)
			end
			for _,t in ipairs(todo) do
				assert(not t[1].uv and not t[2].uv and not t[3].uv)
			end
			if t then
				calcUVBasisAndAddNeighbors(t, tsrc, e, todo)
			end
		end
		for _,t in ipairs(done) do
			assert(t[1].uv and t[2].uv and t[3].uv)
		end
	end

	-- replace?
	self.vts = table()
	for i=1,#self.tris do
		local t = self.tris[i]
		for j=1,3 do
			local src = t[j].uv or {0,0}
			self.vts:insert(matrix{src[1], src[2], 0})
			t[j].vt = #self.vts
			t[j].uv = nil
		end
	end
end

function Mesh:findClosestVertexToMouseRay(pos, dir)
	dir = dir:normalize()
	local bestdot, besti, bestdist
	local eps = 1e-1
	for i,v in ipairs(self.vs) do
		local delta = v - pos
		local dist = delta:dot(dir)
		local dot = dist / delta:norm()
		if dot > 1 - eps then
			if not bestdot
			or (dot >= bestdot and dist < bestdist)
			then
				bestdot = dot
				bestdist = dist
				besti = i
			end
		end
	end
	return besti, bestdist
end

return Mesh
