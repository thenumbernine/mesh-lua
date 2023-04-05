local class = require 'ext.class'
local table = require 'ext.table'
local string = require 'ext.string'
local file = require 'ext.file'
local gl = require 'gl'
local Tex2D = require 'gl.tex2d'
local vec2 = require 'vec.vec2'
local vec3 = require 'vec.vec3'

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

-- truncate arguments
local function tonumber1(x) return tonumber(x) or 0 end

local WavefrontOBJ = class()

function WavefrontOBJ:init(filename)
	local vs = table()
	local vts = table()
	local vns = table()

	self.relpath = file(filename):getdir()

	self.fsForMtl = table()
	self.mtllib = table()
	local curmtl = ''	-- use this instead of 'nil' so the fsForMtl will have a valid key
	self.mtllib[curmtl] = table{name=curmtl}
	assert(file(filename):exists(), "failed to find material file "..filename)
	for line in io.lines(filename) do
		local words = string.split(string.trim(line), '%s+')
		local lineType = words:remove(1):lower()
		if lineType == 'v' then
			assert(#words >= 2)
			vs:insert(vec3(words:mapi(tonumber1):unpack()))
		elseif lineType == 'vt' then
			assert(#words >= 2)
			vts:insert(vec2(words:mapi(tonumber1):unpack()))
		elseif lineType == 'vn' then
			assert(#words >= 2)
			vns:insert(vec3(words:mapi(tonumber1):unpack()))
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
			-- then smooth is on
		elseif lineType == 'g' then
			-- then we start a new group
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
		elseif lineType == 'map_kd' then
			assert(mtl)
			mtl.filename = file(self.relpath)(assert(words[1])).path
			mtl.tex = Tex2D{
				filename = mtl.filename;
				minFilter = gl.GL_NEAREST;
				magFilter = gl.GL_LINEAR;
			}
--[[ maybe later...
		elseif lineType == 'Ka' then
		elseif lineType == 'Kd' then
		elseif lineType == 'Ks' then
		elseif lineType == 'Ns' then
		elseif lineType == 'illum' then
--]]
		end
	end
end

function WavefrontOBJ:draw(args)
	-- this is compiled, so take your time drawing it
	-- just be sure to minimize the hardware cost (ie texture & prim switches)
	gl.glColor3f(1,1,1)
	gl.glPushAttrib(gl.GL_ENABLE_BIT)
	gl.glDisable(gl.GL_CULL_FACE)
	local curtex
	for mtlname, fs in pairs(self.fsForMtl) do
		if mtlname ~= '' then	-- cutting out the '' ... TODO just show this texture-less?
			local mtl = assert(self.mtllib[mtlname])

			assert(mtl.name == mtlname)
			if mtl.tex and not (args and args.disableTextures) then
				curtex = mtl.tex
				curtex:enable()
				curtex:bind()
			elseif curtex then
				curtex:unbind()
				curtex:disable()
				curtex = nil
			end
			gl.glBegin(gl.GL_TRIANGLES)
			if #fs.tris > 0 then
				for _,vis in ipairs(fs.tris) do
					for _,vi in ipairs(vis) do
						if vi.vt then
							gl.glTexCoord2f(self.vts[vi.vt]:unpack())
						end
						if vi.vn then
							gl.glNormal3f(self.vns[vi.vn]:unpack())
						end
						gl.glVertex3f(self.vs[vi.v]:unpack())
					end
				end
			end
			if #fs.quads > 0 then
				for _,vis in ipairs(fs.quads) do
					assert(#vis == 4)
					for _,i in ipairs{3,4,1,4,1,2} do
						local vi = vis[i]
						if vi.vt then
							gl.glTexCoord2f(self.vts[vi.vt]:unpack())
						end
						if vi.vn then
							gl.glNormal3f(self.vns[vi.vn]:unpack())
						end
						gl.glVertex3f(self.vs[vi.v]:unpack())
					end
				end
			end
			gl.glEnd()
		end
	end
	gl.glPopAttrib()
	if curtex then
		curtex:unbind()
		curtex:disable()
	end
end

return WavefrontOBJ
