--  https://en.wikipedia.org/wiki/Wavefront_.obj_file
local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local string = require 'ext.string'
local file = require 'ext.file'
local timer = require 'ext.timer'
local gl = require 'gl'
local Image = require 'image'
local Tex2D = require 'gl.tex2d'
local vec2 = require 'vec.vec2'
local vec3 = require 'vec.vec3'
local vec4 = require 'vec.vec4'
local vector = require 'ffi.cpp.vector'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'

ffi.cdef[[
typedef struct {
	vec3f_t pos;
	vec2f_t tc;
	vec3f_t n;
} obj_vertex_t;
]]

local function pathOfFilename(fn)
	-- find the last index of / in fn
	local lastSlashIndex
	for i=#fn,1,-1 do
		if fn:sub(i,i) == '/' then
			lastSlashIndex = i
			break
		end
	end
	if not lastSlashIndex then return './' end	-- relative to current dir
	return fn:sub(1,lastSlashIndex)
end

-- tonumber but truncate arguments and default to 0
local function tonumber1(x)
	return tonumber(x) or 0
end

local function wordsToNumbers(w)
	return w:mapi(tonumber1):unpack()
end

local function wordsToVec2(w)
	return vec2(wordsToNumbers(w))
end

local function wordsToVec3(w)
	return vec3(wordsToNumbers(w))
end

-- used for colors
local function wordsToColor(w)
	-- TODO error if not 3 or 4?
	local r,g,b,a = w:mapi(function(x) return tonumber(x) end):unpack()
	r = r or 0
	g = g or 0
	b = b or 0
	a = a or 1
	return vec4(r,g,b,a)
end

local WavefrontOBJ = class()

function WavefrontOBJ:init(filename)
	local vs = table()
	local vts = table()
	local vns = table()

	self.relpath = file(filename):getdir()
	self.mtlFilenames = table()

	timer('loading', function()
		self.fsForMtl = {}
		self.mtllib = table()
		local curmtl = ''	-- use this instead of 'nil' so the fsForMtl will have a valid key
		self.mtllib[curmtl] = table{name=curmtl}
		assert(file(filename):exists(), "failed to find material file "..filename)
		for line in io.lines(filename) do
			local words = string.split(string.trim(line), '%s+')
			local lineType = words:remove(1):lower()
			if lineType == 'v' then
				assert(#words >= 2)
				vs:insert(wordsToVec3(words))
			elseif lineType == 'vt' then
				assert(#words >= 2)
				vts:insert(wordsToVec2(words))
			elseif lineType == 'vn' then
				assert(#words >= 2)
				vns:insert(wordsToVec3(words))
			-- TODO lineType == 'vp'
			elseif lineType == 'f' then
				local usingMtl = curmtl
				local vis = table()
				local foundVT = false
				for _,vertexIndexString in ipairs(words) do
					local vertexIndexStringParts = string.split(vertexIndexString, '/')	-- may be empty string
					local vertexIndices = vertexIndexStringParts:mapi(tonumber1)	-- may be nil
					local vi, vti, vni = unpack(vertexIndices)
					if vti then foundVT = true end
					vis:insert{v=vi, vt=vti, vn=vni}
				end

				-- TODO hmm really?
				-- if no vt found then we can still use the Ks Kd etc from the mtl
				-- we just have to take care when drawing it not to have the texture bound
				-- (unlike the other faces in the mtl which do have vt's)
				--if not foundVT then usingMtl = '' end

				local fs = self.fsForMtl[usingMtl]
				if not fs then
					fs = {}
					self.fsForMtl[usingMtl] = fs
				end
				assert(#words >= 3, "got a bad polygon ... does .obj support lines or points?")
				local nvtx = #words
				fs[nvtx] = fs[nvtx] or table()
				fs[nvtx]:insert(vis)
			elseif lineType == 's' then
				-- TODO then smooth is on
				-- for all subsequent polys, or for the entire group (including previously defined polys) ?
			elseif lineType == 'g' then
				-- TODO then we start a new group
			elseif lineType == 'usemtl' then
				curmtl = assert(words[1])
			elseif lineType == 'mtllib' then
				self:loadMtl(assert(words[1]))
			end
		end
		-- check
		for mtlname, fs in pairs(self.fsForMtl) do
			assert(self.mtllib[mtlname], "failed to find mtl "..mtlname)
		end
		-- could've done this up front...
		self.vs = vs
		self.vts = vts
		self.vns = vns
	end)

-- TODO all this per-material-group
-- should meshes have their own vtx lists?
-- or should they just index into a master list (like obj files do?)

	-- and just for kicks, track all edges
	timer('edges', function()
		self.edges = {}
		local function addEdge(a,b)
			if a > b then return addEdge(b,a) end
			self.edges[a] = self.edges[a] or {}
			self.edges[a][b] = true
		end
		for a,b,c in self:triiter() do
			addEdge(a.v,b.v)
			addEdge(a.v,c.v)
			addEdge(b.v,c.v)
		end
	end)

-- [[ TODO all this can go in a superclass for all 3d obj file formats
-- TODO store these?  or only calculate upon demand?
	timer('com0', function()
		self.com0 = self:calcCOM0()
	end)
	timer('com1', function()
		self.com1 = self:calcCOM1()
	end)
	timer('com2', function()
		self.com2 = self:calcCOM2()
	end)
	timer('com3', function()
		self.com3 = self:calcCOM3()
	end)
	-- can only do this with com2 and com3 since they use tris, which are stored per-material
	-- ig i could with edges and vtxs too if I flag them per-material
	for mtlname,mtl in pairs(self.mtllib) do
		mtl.com2 = self:calcCOM2(mtlname)
		mtl.com3 = self:calcCOM3(mtlname)
	end
--]]

	-- now for performance I can either store everything in a packed array
	-- or I can put unique index sets' data in a packed array and store the unique # in an index array (more complex but more space efficient)
	for mtlname, polysPerSides in pairs(self.fsForMtl) do
		local i = 0
		for a,b,c in self:triiter(mtlname) do
			i = i + 3
		end
		local vtxBuf = vector('obj_vertex_t', i)
		i = 0
		for a,b,c in self:triiter(mtlname) do
			for _,vi in ipairs{a,b,c} do
				local v = vtxBuf.v + i
				v.pos:set(self.vs[vi.v]:unpack())
				if vi.vt then
					v.tc:set(self.vts[vi.vt]:unpack())
				end
				if vi.vn then
					v.normal:set(self.vns[vi.vn]:unpack())
				end
				i = i + 1
			end
		end
		self.mtllib[mtlname].vtxBuf = vtxBuf
	end
end

function WavefrontOBJ:loadMtl(filename)
	self.mtlFilenames:insert(filename)
	local mtl
	filename = file(self.relpath)(filename).path
	-- TODO don't assert, and just flag what material files loaded vs didn't?
	assert(file(filename):exists(), "failed to find material file "..filename)
	for line in io.lines(filename) do
		local words = string.split(string.trim(line), '%s+')
		local lineType = words:remove(1):lower()
		if lineType == 'newmtl' then
			mtl = {}
			mtl.name = assert(words[1])
			if self.mtllib[mtl.name] then print("warning: found two mtls of the name "..mtl.name) end
			self.mtllib[mtl.name] = mtl
		-- elseif lineType == 'illum' then
		--[[
			0. Color on and Ambient off
			1. Color on and Ambient on
			2. Highlight on
			3. Reflection on and Ray trace on
			4. Transparency: Glass on, Reflection: Ray trace on
			5. Reflection: Fresnel on and Ray trace on
			6. Transparency: Refraction on, Reflection: Fresnel off and Ray trace on
			7. Transparency: Refraction on, Reflection: Fresnel on and Ray trace on
			8. Reflection on and Ray trace off
			9. Transparency: Glass on, Reflection: Ray trace off
			10. Casts shadows onto invisible surfaces
		--]]
		elseif lineType == 'ka' then	-- ambient color
			assert(mtl)
			mtl.Ka = wordsToColor(words)
		elseif lineType == 'kd' then	-- diffuse color
			assert(mtl)
			mtl.Kd = wordsToColor(words)
		elseif lineType == 'ks' then	-- specular color
			assert(mtl)
			mtl.Ks = wordsToColor(words)
		elseif lineType == 'ns' then	-- specular highlight color
			assert(mtl)
			mtl.Ns = wordsToColor(words)
		-- 'd' = alpha
		-- 'Tr' = 1 - d = opacity
		-- 'Tf' = "transmission filter color"
		-- 'Tf xyz' = same but using CIEXYZ specs
		-- 'Tf spectral filename.rfl [factor]'
		-- 'Ni' = index of refraction aka optical density
		elseif lineType == 'map_kd' then	-- diffuse map
			assert(mtl)
			local localpath = assert(words[1]):gsub('\\', '/')
			mtl.map_Kd = file(self.relpath)(localpath).path
			-- TODO
			-- load textures?
			-- what if the caller isn't using GL?
			-- load images instead?
			-- just store filename and let the caller deal with it?
			mtl.image_Kd = Image(mtl.map_Kd)
			-- TODO here ... maybe I want a console .obj editor that doesn't use GL
			-- in which case ... when should the .obj class load the gl textures?
			-- manually?  upon first draw?  both?
		--elseif lineType == 'map_ks' then	-- specular color map
		--elseif lineType == 'map_ns' then	-- specular highlight map
		--elseif lineType == 'map_bump' or lineType == 'bump' then
		--elseif lineType == 'disp' then
		--elseif lineType == 'decal' then
		-- and don't forget textre map options
		end
	end
end

-- upon ctor the images are loaded (in case the caller isn't using GL)
-- so upon first draw - or upon manual call - load the gl textures
function WavefrontOBJ:loadGLTexs()
	for mtlname, mtl in pairs(self.mtllib) do
		if mtl.image_Kd and not mtl.tex_Kd then
			mtl.tex_Kd = Tex2D{
				image = mtl.image_Kd,
				minFilter = gl.GL_NEAREST,
				magFilter = gl.GL_LINEAR,
			}
		end
	end
end

-- common interface?  for dif 3d format types?
function WavefrontOBJ:vtxiter()
	return coroutine.wrap(function()
		for i,v in ipairs(self.vs) do
			coroutine.yield(v)
		end
	end)
end

-- yields with each material collection for a particular material name
-- default = no name = iterates over all materials
function WavefrontOBJ:mtliter(mtlname)
	return coroutine.wrap(function()
		if mtlname then
			local fs = self.fsForMtl[mtlname]
			if fs then coroutine.yield(fs, mtlname) end
		else
			for mtlname, fs in pairs(self.fsForMtl) do
				coroutine.yield(fs, mtlname)
			end
		end
	end)
end

-- yields with each face in a particular material or in all materials
function WavefrontOBJ:faceiter(mtlname)
	return coroutine.wrap(function()
		for fs in self:mtliter(mtlname) do
			for k=3,table.maxn(fs) do
				for _,vis in ipairs(fs[k]) do
					coroutine.yield(vis)	-- has [1].v [2].v [3].v for vtx indexes
				end
			end
		end
	end)
end

-- yields with vi object which has  .v .vt .vn as indexes into .vs[] .vts[] .vns[]
function WavefrontOBJ:triiter(mtlname)
	return coroutine.wrap(function()
		for vis in self:faceiter(mtlname) do
			for j=2,#vis-1 do
				coroutine.yield(vis[1], vis[j], vis[j+1])
			end
		end
	end)
end

-- same as above, but then yield for each vi individually
function WavefrontOBJ:triindexiter(mtlname)
	return coroutine.wrap(function()
		for i,j,k in self:triiter(mtlname) do
			coroutine.yield(i)
			coroutine.yield(j)
			coroutine.yield(k)
		end
	end)
end

function WavefrontOBJ:draw(args)
	self:loadGLTexs()	-- load if not loaded
	gl.glPushAttrib(gl.GL_ENABLE_BIT)
	gl.glDisable(gl.GL_CULL_FACE)
	local curtex
	for mtlname, fs in pairs(self.fsForMtl) do
		local mtl = assert(self.mtllib[mtlname])
		assert(not mtl or mtl.name == mtlname)
		if mtl.Kd then
			gl.glColor4f(mtl.Kd:unpack())
		else
			gl.glColor4f(1,1,1,1)
		end
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
		-- [[ vertex client arrays
		gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxBuf.v[0].pos.s)
		gl.glTexCoordPointer(2, gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxBuf.v[0].tc.s)
		gl.glNormalPointer(gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxBuf.v[0].n.s)
		gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
		gl.glEnableClientState(gl.GL_TEXTURE_COORD_ARRAY)
		gl.glEnableClientState(gl.GL_NORMAL_ARRAY)
		gl.glDrawArrays(gl.GL_TRIANGLES, 0, mtl.vtxBuf.size)
		gl.glDisableClientState(gl.GL_VERTEX_ARRAY)
		gl.glDisableClientState(gl.GL_TEXTURE_COORD_ARRAY)
		gl.glDisableClientState(gl.GL_NORMAL_ARRAY)
		--]]
		if args.endMtl then args.endMtl(mtl) end
	end
	gl.glPopAttrib()
	if curtex then
		curtex:unbind()
		curtex:disable()
	end
	require 'gl.report''here'
end

-- calculate COM by 0-forms (vertexes)
function WavefrontOBJ:calcCOM0()
	return self.vs:sum() / #self.vs
end

-- calculate COM by 1-forms (edges)
-- depend on self.edges being stored
function WavefrontOBJ:calcCOM1()
	local totalCOM = vec3()
	local totalArea = 0
	for a,bs in pairs(self.edges) do
		for b in pairs(bs) do
			local v1 = self.vs[a]
			local v2 = self.vs[b]
			-- volume = *<Q,Q> = *(Q∧*Q) where Q = (b-a)
			-- for 1D, volume = |b-a|
			local area = (v1 - v2):length()
			local com = (v1 + v2) * .5
			totalCOM = totalCOM + com * area
			totalArea = totalArea + area
		end
	end
	return totalCOM / totalArea
end

-- calculate COM by 2-forms (triangles)
function WavefrontOBJ:calcCOM2(mtlname)
	local totalCOM = vec3()
	local totalArea = 0
	for i,j,k in self:triiter(mtlname) do
		local a = self.vs[i.v]
		local b = self.vs[j.v]
		local c = self.vs[k.v]
		-- volume = *<Q,Q> = *(Q∧*Q) where Q = (b-a) ∧ (c-a)
		-- for 2D, volume = |(b-a)x(c-a)|
		local ab = b - a
		local ac = c - a
		local area = .5 * ab:cross(ac):length()
		local com = (a + b + c) * (1/3)
		totalCOM = totalCOM + com * area
		totalArea = totalArea + area
	end
	return totalCOM / totalArea
end

-- calculate COM by 3-forms (enclosed volume)
function WavefrontOBJ:calcCOM3(mtlname)
	local totalCOM = vec3()
	local totalVolume = 0
	for i,j,k in self:triiter(mtlname) do
		local a = self.vs[i.v]
		local b = self.vs[j.v]
		local c = self.vs[k.v]

		-- using [a,b,c,0] as the 4 pts of our tetrahedron
		-- volume = *<Q,Q> = *(Q∧*Q) where Q = (a-0) ∧ (b-0) ∧ (c-0)
		-- for 3D, volume = det|a b c|
		local com = (a + b + c) * (1/4)

		local volume = 0
		volume = volume + a[1] * b[2] * c[3]
		volume = volume + a[2] * b[3] * c[1]
		volume = volume + a[3] * b[1] * c[2]
		volume = volume - c[1] * b[2] * a[3]
		volume = volume - c[2] * b[3] * a[1]
		volume = volume - c[3] * b[1] * a[2]

		totalCOM = totalCOM + com * volume
		totalVolume = totalVolume + volume
	end
	return totalCOM / totalVolume
end

-- calculates overall volume
function WavefrontOBJ:calcVolume()
	local volume = 0
	for i,j,k in self:triiter() do
		local a = self.vs[i.v]
		local b = self.vs[j.v]
		local c = self.vs[k.v]

		volume = volume + a[1] * b[2] * c[3]
		volume = volume + a[2] * b[3] * c[1]
		volume = volume + a[3] * b[1] * c[2]
		volume = volume - c[1] * b[2] * a[3]
		volume = volume - c[2] * b[3] * a[1]
		volume = volume - c[3] * b[1] * a[2]
	end
	if volume < 0 then volume = -volume end
	volume = volume / 6
	return volume
end

function WavefrontOBJ:save(filename)
	local o = assert(file(filename):open'w')
	-- TODO write smooth flag, groups, etc
	for _,mtl in ipairs(self.mtlFilenames) do
		o:write('mtllib ', mtl, '\n')
	end
	for _,v in ipairs(self.vs) do
		o:write('v ', table.concat(v, ' '), '\n')
	end
	for _,vt in ipairs(self.vts) do
		o:write('vt ', table.concat(vt, ' '), '\n')
	end
	for _,vn in ipairs(self.vns) do
		o:write('vn ', table.concat(vn, ' '), '\n')
	end
	for _,mtl in ipairs(table.keys(self.fsForMtl):sort()) do
		o:write('usemtl ', mtl, '\n')
		local fs = self.fsForMtl[mtl]
		for k=3,table.maxn(fs) do
			for _,vis in ipairs(fs[k]) do
				o:write('f ', table.mapi(vis, function(vi)
					local vs = table{vi.v, vi.vt, vi.vn}
					for i=1,vs:maxn() do vs[i] = vs[i] or '' end
					return vs:concat'/'
				end):concat' ', '\n')
			end
		end
	end
	o:close()
end

return WavefrontOBJ
