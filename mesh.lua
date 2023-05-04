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
	vec3f_t texcoord;
	vec3f_t normal;

	// per-triangle stats (duplicated 3x per-vertex)
	vec3f_t com;		//com of tri containing this vertex.  only good for un-indexed drawing.
} obj_vertex_t;
]]

local function triArea(a,b,c)
	local ab = b - a
	local ac = c - a
	local n = ab:cross(ac)
	return .5 * n:norm()
end

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

local Mesh = class()

function Mesh:init(filename)
	-- TODO new system:
	self.vtxCPUBuf = vector'obj_vertex_t'
	self.triIndexBuf = vector'int32_t'
	
	-- or maybe store indexes too
	-- then the rest pack/unpack upon obj save/load
	self.vs = table()
	self.vts = table()
	self.vns = table()
	self.tris = table() -- triangulation of all faces
	
	self.mtllib = {[''] = {}}
end

function Mesh:prepare()
-- [[ calculate bbox.
-- do this before merging vtxs.
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
	local box3 = require 'vec.box3'
	self.bbox = box3(-math.huge)
	for _,v in ipairs(self.vs) do
		self.bbox:stretch(v)
	end
end

function Mesh:mergeMatchingVertexes()
	if not self.bbox then self:calcBBox() end
	-- ok the bbox hyp is 28, the smallest maybe valid dist is .077, and everything smalelr is 1e-6 ...
	-- that's a jump from 1/371 to 1/20,000,000
	-- so what's the smallest ratio I should allow?  maybe 1/1million?
	local bboxCornerDist = (self.bbox.max - self.bbox.min):norm()
	local vtxMergeThreshold = bboxCornerDist * 1e-6
print('vtxMergeThreshold', vtxMergeThreshold)
	print('before merge vtx count', #self.vs, 'tri count', self.triIndexBuf.size)
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
	print('after merge vtx count', #self.vs, 'tri count', self.triIndexBuf.size)
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
				length = (self.vtxCPUBuf.v[a-1].pos - self.vtxCPUBuf.v[b-1].pos):norm(),
			}
			local e = self.edges[a][b]
			e.tris:insert(t)
			t.edges:insert(e)
		end
		for i=0,self.triIndexBuf.size-1,3 do
			local a = self.triIndexBuf.v[i]
			local b = self.triIndexBuf.v[i+1]
			local c = self.triIndexBuf.v[i+2]
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
	assert(from >= 0 and from <= self.vtxCPUBuf.size)
	assert(to >= 0 and to <= self.vtxCPUBuf.size)
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
		local t = self.triIndexBuf.v + j
		for j=2,1,-1 do
			if t[j] == t[j-1] then
--print('removing degenerate tri '..i..' with duplicate vertices')
				self:removeTri(i)
				break
			end
		end
	end
end

function Mesh:removeTri(i)
	self.triIndexBuf:erase(self.triIndexBuf.v + i, self.triIndexBuf.v + i + 3)
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
	assert(vi >= 0 and vi < self.vtxCPUBuf.size)
	self.vtxCPUBuf:erase(self.vtxCPUBuf.v + i, self.vtxCPUBuf.v + i + 1)
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
			usedVs[self.triIndexBuf.v[j]] = true
		end
	end)
	timer('removing unused vtxs', function()
		print('before removing, #vs', self.vtxCPUBuf.size)
		for i=self.vtxCPUBuf.size-1,0,-1 do
			if not usedVs[i] then
				self:removeVertex(i)
			end
		end
		print('after removing, #vs', self.vtxCPUBuf.size)
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
		return 1, self.triIndexBuf.size/3
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

-- calculate COM by 0-forms (vertexes)
function Mesh:calcCOM0()
	local result = vec3f()
	for i=0,self.vtxCPUBuf.size-1 do
		result = result + self.vtxCPUBuf.v[i].pos
	end
	result = result / self.vtxCPUBuf.size
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculate COM by 1-forms (edges)
-- depend on self.edges being stored
function Mesh:calcCOM1()
	local totalCOM = vec3f()
	local totalLen = 0
	for a,bs in pairs(self.edges) do
		for b in pairs(bs) do
			local v1 = self.vtxCPUBuf.v[a-1].pos
			local v2 = self.vtxCPUBuf.v[b-1].pos
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
	local totalCOM = vec3f()
	local totalArea = 0
	local i1, i2 = self:getTriIndexesForMaterial(mtlname)
	for i=i1,i2 do
		local a, b, c = self:getTriVtxPos(3*(i-1))
		local com = (a + b + c) * (1/3)
		local area = triArea(a, b, c)
		totalCOM = totalCOM + com * area
		totalArea = totalArea + area
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
	local totalCOM = vec3f()
	local totalVolume = 0
	local i1, i2 = self:getTriIndexesForMaterial(mtlname)
	for i=i1,i2 do
		local a, b, c = self:getTriVtxPos(3*(i-1))

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
	for i=0,self.triIndexBuf.size-1,3 do
		totalVolume = totalVolume + tetradVolume(self:getTriVtxPos(i))
	end
	if totalVolume < 0 then totalVolume = -totalVolume end
	return totalVolume
end


-- regenerate the vertex normals based on the face normals, weighted average by face area
function Mesh:regenNormals()
	-- calculate vertex normals
	-- TODO store this?  in its own self.vn2s[] or something?
--print('zeroing vertex normals')
	local vtxnormals = vector('vec3f_t', #self.vs)
--print('accumulating triangle normals into vertex normals')
	for i=0,self.triIndexBuf.size-1,3 do
		local ia = self.triIndexBuf.v[i]
		local ib = self.triIndexBuf.v[i+1]
		local ic = self.triIndexBuf.v[i+2]
		-- not sure what i'm doing with these ...
		-- cache or regen?
		local a = self.vtxCPUBuf.v[ia].pos
		local b = self.vtxCPUBuf.v[ib].pos
		local c = self.vtxCPUBuf.v[ic].pos
		local area = triArea(a, b, c)
		local normal = (b - a):cross(c - b)
		if normal:norm() < 1e-7 then
			normal = vec3f(0,0,0)
		else
			normal = normal:normalize()
			if not math.isfinite(normal:normSq()) then
				normal = vec3f(0,0,0)
			end
		end

		if math.isfinite(normal:normSq()) then
			local normalArea = normal * area
			vtxnormals.v[ia] = vtxnormals.v[ia] + normalArea
			vtxnormals.v[ib] = vtxnormals.v[ib] + normalArea
			vtxnormals.v[ic] = vtxnormals.v[ic] + normalArea
		end
	end
--print('normals vertex normals')
	for i=0,vtxnormals.size-1 do
		if vtxnormals.v[i]:norm() > 1e-7 then
			vtxnormals.v[i] = vtxnormals.v[i]:normalize()
		end
--print(k, vtxnormals[i])
	end

	for i=0,self.vtxCPUBuf.size-1 do
		self.vtxCPUBuf.v[i].normal = vtxnormals.v[i]
	end
	if self.vtxBuf then
		self.vtxBuf:updateData(0, ffi.sizeof'obj_vertex_t' * self.vtxCPUBuf.size, self.vtxCPUBuf.v)
	end
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

	-- mtl will just index into this.
	-- why does mtl store a list of tri indexes?  it should just store an offset

--print('creating array buffer of size', self.vtxCPUBuf.size)
	self.vtxBuf = GLArrayBuffer{
		size = self.vtxCPUBuf.size * ffi.sizeof'obj_vertex_t',
		data = self.vtxCPUBuf.v,
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
			stride = ffi.sizeof'obj_vertex_t',
			offset = ffi.offsetof('obj_vertex_t', info.name),
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
		gl.glTexCoordPointer(3, gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].texcoord.s)
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
		gl.glVertexAttribPointer(args.shader.attrs.texcoord.loc, 3, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].texcoord.s)
		gl.glVertexAttribPointer(args.shader.attrs.normal.loc, 3, gl.GL_FLOAT, gl.GL_TRUE, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].normal.s)
		gl.glEnableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.texcoord.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.normal.loc)
		gl.glDrawArrays(gl.GL_TRIANGLES, 0, mtl.vtxCPUBuf.size)
		gl.glDisableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.texcoord.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.normal.loc)
		--]]
		-- [[ vao ... getting pretty tightly coupled with the view.lua file ...
		if mtl.triCount > 0 then
			self.vao:use()
			gl.glDrawElements(gl.GL_TRIANGLES, mtl.triCount * 3, gl.GL_UNSIGNED_INT, self.triIndexBuf.v + (mtl.triFirstIndex-1) * 3)
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
			local offset = vec3f()
			for i,t in ipairs(edge.tris) do
				local va, vb, vc = self:getTriVtxPos(3*(i-1))
				local tcom = (va + vb + vc) * (1/3)
				-- get mtl for tri, then do groupExplodeDist too
				-- matches the shader in view.lua
				local groupExplodeOffset = (t.mtl.com3 - self.com3) * groupExplodeDist
				local triExplodeOffset = (tcom - t.mtl.com3) * triExplodeDist
				offset = offset + groupExplodeOffset + triExplodeOffset
			end
			offset = offset / #edge.tris
			gl.glVertex3fv((self.vtxCPUBuf.v[a-1].pos + offset).s)
			gl.glVertex3fv((self.vtxCPUBuf.v[b-1].pos + offset).s)
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
	for i=0,self.vtxCPUBuf.size-1 do
		local v = self.vtxCPUBuf.v[i].pos
		-- avg of explode offsets of all touching tris
		local offset = vec3f()
		-- get mtl for tri, then do groupExplodeDist too
		-- matches the shader in view.lua
		local triExplodeOffset = (v - self.com3) * triExplodeDist
		offset = offset + triExplodeOffset
		gl.glVertex3fv((v + offset).s)
	end
	gl.glEnd()
	gl.glPointSize(1)
end


function Mesh:drawVertexNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for i=0,self.vtxCPUBuf.size-1 do
		local v = self.vtxCPUBuf.v[i]
		gl.glVertex3f(v.pos:unpack())
		gl.glVertex3f((v.pos + v.normal):unpack())
	end
	gl.glEnd()
end

-- 0-based
function Mesh:getTriVtxPos(i)
	local ia = self.triIndexBuf.v[i]
	local ib = self.triIndexBuf.v[i+1]
	local ic = self.triIndexBuf.v[i+2]
	local va = self.vtxCPUBuf.v[ia].pos
	local vb = self.vtxCPUBuf.v[ib].pos
	local vc = self.vtxCPUBuf.v[ic].pos
	return va, vb, vc
end

function Mesh:drawTriNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for i=0,self.triIndexBuf.size-1,3 do
		local a, b, c = self:getTriVtxPos(i)
		local normal = (b - a):cross(c - b)
		local com = (a + b + c) * (1/3)
		gl.glVertex3fv(com.s)
		gl.glVertex3fv((com + normal).s)
	end
	gl.glEnd()
end

function Mesh:findClosestVertexToMouseRay(pos, dir, fwd, cosEpsAngle)
	-- assumes dir is 1 unit fwd along the view fwd
	--dir = dir:normalize()
	local dirlen = dir:norm()
	local bestdot, besti, bestdepth
	for i=0,self.vtxCPUBuf.size-1 do
		local v = self.vtxCPUBuf.v[i].pos
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
