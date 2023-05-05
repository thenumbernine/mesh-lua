--  https://en.wikipedia.org/wiki/Wavefront_.obj_file
local file = require 'ext.file'
local class = require 'ext.class'
local table = require 'ext.table'
local string = require 'ext.string'
local timer = require 'ext.timer'
local math = require 'ext.math'
local vector = require 'ffi.cpp.vector'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'
local Image = require 'image'
local Mesh = require 'mesh'

local function wordsToVec3(w)
	return vec3f(
		tonumber(w[1]) or 0,
		tonumber(w[2]) or 0,
		tonumber(w[3]) or 0
	)
end

-- used for colors
local function wordsToColor(w)
	-- TODO error if not 3 or 4?
	local r,g,b,a = w:mapi(function(x) return tonumber(x) end):unpack(1, 4)
	r = r or 0
	g = g or 0
	b = b or 0
	a = a or 1
	return vec4f(r,g,b,a)
end


local OBJLoader = class()

function OBJLoader:load(filename)
	local mesh = Mesh()
	
	local vs = table()
	local vts = table()
	local vns = table()
	local tris = table()

	mesh.relpath = file(filename):getdir()
	mesh.mtlFilenames = table()

--timer('loading', function()

	-- map of materials
	mesh.mtllib = {}
	local curmtl = ''
	mesh.mtllib[curmtl] = {
		name = curmtl,
	}
	assert(file(filename):exists(), "failed to find material file "..filename)
	for line in io.lines(filename) do
		local words = string.split(string.trim(line), '%s+')
		local lineType = words:remove(1):lower()
		if lineType == 'v' then
			assert(#words >= 2)
			vs:insert(wordsToVec3(words))
		elseif lineType == 'vt' then
			assert(#words >= 2)
			vts:insert(wordsToVec3(words))
		elseif lineType == 'vn' then
			assert(#words >= 2)
			vns:insert(wordsToVec3(words))
		-- TODO lineType == 'vp'
		elseif lineType == 'f' then
			local vis = table()
			local foundVT = false
			for _,vertexIndexString in ipairs(words) do
				local vertexIndexStringParts = string.split(vertexIndexString, '/')	-- may be empty string
				local vertexIndices = vertexIndexStringParts:mapi(function(x) return tonumber(x) end)	-- may be nil
				local vi, vti, vni = unpack(vertexIndices, 1, 3)
				if vti then foundVT = true end
				vis:insert{v=vi, vt=vti, vn=vni}
			end
			local mtl = mesh.mtllib[curmtl]
			mtl.name = curmtl
			assert(#words >= 3, "got a bad polygon ... does .obj support lines or points?")
			for i=2,#words-1 do
				-- store a copy of the vertex indices per triangle index
				local t = {
					table(vis[1]):setmetatable(nil),
					table(vis[i]):setmetatable(nil),
					table(vis[i+1]):setmetatable(nil),
				}
				tris:insert(t)
				-- keys:
				t.index = #tris+1	-- so far only used for debugging
				t.mtl = mtl
				if not mtl.triFirstIndex then mtl.triFirstIndex = #tris-1 end
				mtl.triCount = #tris - mtl.triFirstIndex + 1
			end
		elseif lineType == 's' then
			-- TODO then smooth is on
			-- for all subsequent polys, or for the entire group (including previously defined polys) ?
		elseif lineType == 'g' then
			-- TODO then we start a new named group
		elseif lineType == 'o' then
			-- TODO then we start a new named object
		elseif lineType == 'usemtl' then
			curmtl = assert(words[1])
			local mtl = mesh.mtllib[curmtl]
			if not mtl then
				print("failed to find material "..curmtl)
				mtl = {}
				mesh.mtllib[curmtl] = mtl
			end
			assert(not mtl.triFirstIndex)
			mtl.triFirstIndex = #tris
			mtl.triCount = 0
		elseif lineType == 'mtllib' then
			-- TODO this replaces %s+ with space ... so no tabs or double-spaces in filename ...
			self:loadMtl(words:concat' ', mesh)
		end
	end

--end)

	for _,m in pairs(mesh.mtllib) do
		if not m.triFirstIndex then
			m.triFirstIndex = 0
			m.triCount = 0
		end
	end

	-- calc vtx normals before or after grouping redundant vtxs?

	local indexForVtx = {}	-- from 'v,vt,vn'
	local vtxCPUBuf = vector'obj_vertex_t'	-- vertex structure
	local triIndexBuf = vector'int32_t'		-- triangle indexes
	for _,t in ipairs(tris) do
		for j,tj in ipairs(t) do
			local k = table.concat({tj.v, tj.vt, tj.vn},',')
			local i = indexForVtx[k]	-- 0-based
			if not i then
				i = vtxCPUBuf.size
				indexForVtx[k] = i
				local dst = vtxCPUBuf:emplace_back()
				dst.pos = assert(vs[tj.v])
				if tj.vt then
					dst.texcoord = assert(vts[tj.vt])
				else
					dst.texcoord:set(0,0,0)
				end
				if tj.vn then
					dst.normal = assert(vns[tj.vn])
				else
					dst.normal:set(0,0,0)
				end
			end
			triIndexBuf:push_back(i)
			tj.v = i+1
			tj.vt = i+1
			tj.vn = i+1
		end
	end
--print('#unique vertexes', vtxCPUBuf.size)
--print('#unique triangles', triIndexBuf.size)

	mesh.vtxCPUBuf = vtxCPUBuf
	mesh.triIndexBuf = triIndexBuf

	-- while we're here, regenerate the vs vts vns from their reduced triIndexBuf values
	mesh.tris = tris

	return mesh
end

function OBJLoader:loadMtl(filename, mesh)
	mesh.mtlFilenames:insert(filename)
	local mtl
	filename = file(mesh.relpath)(filename).path
	-- TODO don't assert, and just flag what material files loaded vs didn't?
	if not file(filename):exists() then
		io.stderr:write("failed to find material file "..filename..'\n')
		return
	end
	for line in io.lines(filename) do
		local words = string.split(string.trim(line), '%s+')
		local lineType = words:remove(1):lower()
		if lineType == 'newmtl' then
			mtl = {}
			mtl.name = assert(words[1])
			-- TODO if a mtllib comes after a face then this'll happen:
			if mesh.mtllib[mtl.name] then print("warning: found two mtls of the name "..mtl.name) end
			mesh.mtllib[mtl.name] = mtl
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
		elseif lineType == 'ns' then	-- specular exponent
			assert(mtl)
			mtl.Ns = tonumber(words[1]) or 1
		-- 'd' = alpha
		-- 'Tr' = 1 - d = opacity
		-- 'Tf' = "transmission filter color"
		-- 'Tf xyz' = same but using CIEXYZ specs
		-- 'Tf spectral filename.rfl [factor]'
		-- 'Ni' = index of refraction aka optical density
		elseif lineType == 'map_kd' then	-- diffuse map
			assert(mtl)
			local function getTexOpts(w)
				local opts = {}
				local found
				repeat
					found = false
					local function parse(n)
						w:remove(1)
						local res = table()
						for i=1,n do
							local v = w[1]
							if n == 3 then	-- colors have optionally 1 thru 3 numeric args
								v = tonumber(v)
								if not v then break end
							else
								v = tonumber(v) or v
							end
							w:remove(1)
							res:insert(v)
						end
						found = true
						return res
					end
					local valid = {
						blendu = 1,
						blendv = 1,
						boost = 1,
						mm = 2,	-- only 2 numeric
						o = 3,	-- up to 3 numeric
						s = 3,	-- up to 3 numeric
						t = 3,	-- up to 3 numeric
						texres = 1,
						clamp = 1,
						bm = 1,
						imfchan = 1,
						type = 1,	-- for reflection maps only
					}
					local l = w[1]:lower()
					if l:sub(1,1) == '-' then
						local k = l:sub(2)
						local v = valid[k]
						if v then
							opts[k] = parse(v)
						end
					end
				until not found
			end
			local opts = getTexOpts(words)
			-- TODO this replaces %s+ with space ... so no tabs or double-spaces in filename ...
			local localpath = words:concat' '
			localpath = localpath:gsub('\\\\', '/')	-- why do I see windows mtl files with \\ as separators instead of just \ (let alone /) ?  is \\ a thing for mtl windows?
			localpath = localpath:gsub('\\', '/')
			local path = file(mesh.relpath)(localpath)
			if not path:exists() then
				print("couldn't load map_Kd "..tostring(path))
			else
				mtl.map_Kd = path.path
				-- TODO
				-- load textures?
				-- what if the caller isn't using GL?
				-- load images instead?
				-- just store filename and let the caller deal with it?
				mtl.image_Kd = Image(mtl.map_Kd)
--print('loaded map_Kd '..mtl.map_Kd..' as '..mtl.image_Kd.width..' x '..mtl.image_Kd.height..' x '..mtl.image_Kd.channels..' ('..mtl.image_Kd.format..')')
				-- TODO here ... maybe I want a console .obj editor that doesn't use GL
				-- in which case ... when should the .obj class load the gl textures?
				-- manually?  upon first draw?  both?
			end
		--elseif lineType == 'map_ks' then	-- specular color map
		--elseif lineType == 'map_ns' then	-- specular highlight map
		--elseif lineType == 'map_bump' or lineType == 'bump' then
		--elseif lineType == 'disp' then
		--elseif lineType == 'decal' then
		-- and don't forget textre map options
		end
	end
end

-- only saves the .obj, not the .mtl
function OBJLoader:save(filename, mesh)
	local o = assert(file(filename):open'w')
	-- TODO write smooth flag, groups, etc
	for _,mtl in ipairs(mesh.mtlFilenames) do
		o:write('mtllib ', mtl, '\n')
	end
	for i=0,mesh.vtxCPUBuf.size-1 do
		local v = mesh.vtxCPUBuf.v[i].pos
		o:write('v ',v.x,' ',v.y,' ',v.z,'\n')
	end
	for i=0,mesh.vtxCPUBuf.size-1 do
		local v = mesh.vtxCPUBuf.v[i].texcoord
		o:write('vt ',v.x,' ',v.y,' ',v.z,'\n')
	end
	for i=0,mesh.vtxCPUBuf.size-1 do
		local v = mesh.vtxCPUBuf.v[i].normal
		o:write('vn ',v.x,' ',v.y,' ',v.z,'\n')
	end
	local mtlnames = table.keys(mesh.mtllib):sort()
	assert(mtlnames[1] == '')	-- should always be there
	for _,mtlname in ipairs(mtlnames) do
		local mtl = mesh.mtllib[mtlname]
		local usemtlWritten = false
		local function writeMtlName()
			if mtlname == '' then return end
			if usemtlWritten then return end
			usemtlWritten = true
			o:write('usemtl ', mtlname, '\n')
		end
		local lastt
		local lasttnormal
		local vis
		local function writeFaceSoFar()
			if not vis then return end
			writeMtlName()
			o:write('f ', table.mapi(vis, function(vi)
				return table{vi, vi, vi}:concat'/'
			end):concat' ', '\n')
			vis = nil
		end
		for i=mtl.triFirstIndex,mtl.triFirstIndex+mtl.triCount-1 do
			local t = mesh.triIndexBuf.v + 3*i
			local a,b,c = mesh:getTriVtxPos(3*i)
			local area = mesh.triArea(a,b,c)
			if area > 0 then
				local normal = mesh.triNormal(a,b,c)
				if lastt
				and t[0] == lastt[0]
				and t[1] == lastt[2]
				-- same plane
				and normal:dot(lasttnormal) > 1 - 1e-3
				and math.abs((mesh.vtxCPUBuf.v[t[2]].pos - mesh.vtxCPUBuf.v[lastt[2]].pos):dot(normal)) < 1e-3
				then
					-- continuation of the last face
					vis:insert(t[2])
				else
					writeFaceSoFar()
					vis = table{t[0], t[1], t[2]}
				end
				lastt = t
				lasttnormal = normal
			end
		end
		writeFaceSoFar()
	end
	o:close()
end

return OBJLoader
