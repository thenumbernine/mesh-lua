--  https://en.wikipedia.org/wiki/Wavefront_.obj_file
local class = require 'ext.class'
local table = require 'ext.table'
local string = require 'ext.string'
local file = require 'ext.file'
local gl = require 'gl'
local Tex2D = require 'gl.tex2d'
local vec2 = require 'vec.vec2'
local vec3 = require 'vec.vec3'
local vec4 = require 'vec.vec4'

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
			if not foundVT then usingMtl = '' end
			local fs = self.fsForMtl[usingMtl]
			if not fs then
				fs = table()
				fs.tris = table()
				fs.quads = table()
				self.fsForMtl[usingMtl] = fs
			end
			if #words == 3 then
				fs.tris:insert(vis)
			elseif #words == 4 then
				fs.quads:insert(vis)
			else
				error("got unknown number of vertices on this face: "..#words)
			end
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
end

function WavefrontOBJ:loadMtl(filename)
	local mtl
	filename = file(self.relpath)(filename).path
	assert(file(filename):exists(), "failed to find material file "..filename)
	for line in io.lines(filename) do
		local words = string.split(string.trim(line), '%s+')
		local lineType = words:remove(1):lower()
		if lineType == 'newmtl' then
			mtl = table()
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
			mtl.map_Kd = file(self.relpath)(assert(words[1])).path
			mtl.tex_Kd = Tex2D{
				filename = mtl.map_Kd,
				minFilter = gl.GL_NEAREST;
				magFilter = gl.GL_LINEAR;
			}
			-- old compat
			mtl.tex = mtl.tex_Kd
			mtl.filename = mtl.map_Kd
		--elseif lineType == 'map_ks' then	-- specular color map
		--elseif lineType == 'map_ns' then	-- specular highlight map
		--elseif lineType == 'map_bump' or lineType == 'bump' then
		--elseif lineType == 'disp' then
		--elseif lineType == 'decal' then
		-- and don't forget textre map options
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

function WavefrontOBJ:faceiter()
	return coroutine.wrap(function()
		for mtlname, fs in pairs(self.fsForMtl) do
			if #fs.tris > 0 then
				for _,vis in ipairs(fs.tris) do
					coroutine.yield(vis)	-- has [1].v [2].v [3].v for vtx indexes
				end
			end
			if #fs.quads > 0 then
				for _,vis in ipairs(fs.quads) do
					coroutine.yield(vis)	-- has [1].v [2].v [3].v [4].v for vtx indexes
				end
			end
		end
	end)
end

-- yields with vi object which has  .v .vt .vn as indexes into .vs[] .vts[] .vns[]
function WavefrontOBJ:triiter()
	return coroutine.wrap(function()
		for vis in self:faceiter() do
			if #vis == 3 then
				coroutine.yield(vis[1], vis[2], vis[3])
			elseif #vis == 4 then
				coroutine.yield(vis[1], vis[2], vis[4])
				coroutine.yield(vis[1], vis[3], vis[4])
			else
				error("here")
			end
		end
	end)
end

-- same as above, but then yield for each vi individually
function WavefrontOBJ:triindexiter()
	return coroutine.wrap(function()
		for i,j,k in self:triiter() do
			coroutine.yield(i)
			coroutine.yield(j)
			coroutine.yield(k)
		end
	end)
end

function WavefrontOBJ:draw(args)
	gl.glPushAttrib(gl.GL_ENABLE_BIT)
	gl.glDisable(gl.GL_CULL_FACE)
	local curtex
	for mtlname, fs in pairs(self.fsForMtl) do
		local mtl = assert(self.mtllib[mtlname])
		assert(not mtl or mtl.name == mtlname)
		if mtl and mtl.tex_Kd and not (args and args.disableTextures) then
			-- TODO use .Ka, Kd, Ks, Ns, etc
			-- with fixed pipeline?  opengl lighting?
			-- with a shader in the wavefrontobj lib? 
			-- with ... nothing?
			curtex = mtl.tex_Kd
			curtex:enable()
			curtex:bind()
			gl.glColor3f(1,1,1)
		elseif curtex then
			curtex:unbind()
			curtex:disable()
			curtex = nil
			if mtlname ~= '' then
				gl.glColor3f(1,1,1)
			else
				gl.glColor3f(0,0,0)
			end
		end
		gl.glBegin(gl.GL_TRIANGLES)
		for vi in self:triindexiter() do
			if vi.vt then
				gl.glTexCoord2f(self.vts[vi.vt]:unpack())
			end
			if vi.vn then
				gl.glNormal3f(self.vns[vi.vn]:unpack())
			end
			gl.glVertex3f(self.vs[vi.v]:unpack())
		end
		gl.glEnd()
	end
	gl.glPopAttrib()
	if curtex then
		curtex:unbind()
		curtex:disable()
	end
end

return WavefrontOBJ
