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
	
	-- TODO get rid of this old method	
	mesh.tris = table()

timer('loading', function()

	-- mesh groups / materials
	local group
	
	mesh.relpath = file(filename):getdir()
	mesh.mtlFilenames = table()

	local function ensureGroup()
		if group then return end
		-- make default
		group = {
			name = '',
			triFirstIndex = #mesh.tris,
			triCount = 0,
		}
		mesh.groups:insert(group)
	end

	assert(file(filename):exists(), "failed to find WavefrontObj file "..filename)
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
			ensureGroup()
			assert(#words >= 3, "got a bad polygon ... does .obj support lines or points?")
			for i=2,#words-1 do
				-- store a copy of the vertex indices per triangle index
				-- v vt vn are 1-based
				local t = {
					table(vis[1]):setmetatable(nil),
					table(vis[i]):setmetatable(nil),
					table(vis[i+1]):setmetatable(nil),
				}
				mesh.tris:insert(t)
				-- keys:
				t.index = #mesh.tris+1	-- so far only used for debugging
				t.group = assert(group)
				group.triCount = #mesh.tris - group.triFirstIndex
			end
		elseif lineType == 's' then
			-- TODO then smooth is on
			-- for all subsequent polys, or for the entire group (including previously defined polys) ?
		elseif lineType == 'g' then
			-- TODO then we start a new named group
		elseif lineType == 'o' then
			-- TODO then we start a new named object
		elseif lineType == 'usemtl' then
			local mtlname = assert(words[1])
			group = self:makeOrFindGroup(mtlname, mesh)
		elseif lineType == 'mtllib' then
			-- TODO this replaces %s+ with space ... so no tabs or double-spaces in filename ...
			self:loadMtl(words:concat' ', mesh)
		end
	end
end)

	if self.verbose then
		print('removing unused materials...')
	end
	for i=#mesh.groups,1,-1 do
		local g = mesh.groups[i]
		if not g.triCount or g.triCount == 0 then
			mesh.groups:remove(i)
		end
	end

	if self.verbose then
		print'allocating vertex and index buffers...'
	end
	local vtxs = vector('MeshVertex_t', 3*#mesh.tris)	-- vertex structure
	local triIndexBuf = vector('int32_t', 3*#mesh.tris)		-- triangle indexes
	-- hmm init capacity arg?
	vtxs:resize(0)
	triIndexBuf:resize(0)
	if self.verbose then
		print'calculating vertex and index buffers...'
	end

	-- [=[ optimize?
	local indexForVtx = {}	-- from 'v,vt,vn'
	for ti,t in ipairs(mesh.tris) do
		for j,tj in ipairs(t) do
			local k = tj.v..','..(tj.vt or '0')..','..(tj.vn or '0')
			-- [[ allocating way too much ...
			local i = indexForVtx[k]	-- 0-based
			--]]
			--[[
			for ti2=1,ti do
				local t2 = mesh.tris[ti2]
				for j2=1,(ti==ti2 and j-1 or 3) do
					local tj2 = t2[j2]
					... how would this be any less memory? but lots slower.
				end
			end
			--]]
			if not i then
				i = vtxs.size
				indexForVtx[k] = i
				local dst = vtxs:emplace_back()
				dst.pos:set(assert(vs[tj.v]):unpack())
				if tj.vt then
					dst.texcoord:set(assert(vts[tj.vt]):unpack())
				else
					dst.texcoord:set(0,0,0)
				end
				if tj.vn then
					dst.normal:set(assert(vns[tj.vn]):unpack())
				else
					dst.normal:set(0,0,0)
				end
			end
			triIndexBuf:emplace_back()[0] = i
			tj.v = i+1
			tj.vt = i+1
			tj.vn = i+1
		end
	end
	if self.verbose then
		print('#unique vertexes', vtxs.size)
		print('#unique triangles', triIndexBuf.size)
	end
	--]=]

	mesh.vtxs = vtxs
	mesh.triIndexBuf = triIndexBuf
	
	if self.verbose then
		print'done'
	end
	return mesh
end

function OBJLoader:loadMtl(filename, mesh)
timer('loading mtl file', function()
	-- TODO don't store mtlFilenames
	mesh.mtlFilenames:insert(filename)
	
	local group
	filename = file(mesh.relpath)(filename).path
	-- TODO don't assert, and just flag what material files loaded vs didn't?
	if not file(filename):exists() then
		io.stderr:write("failed to find WavefrontObj material file "..filename..'\n')
		return
	end
	for line in io.lines(filename) do
		local words = string.split(string.trim(line), '%s+')
		local lineType = words:remove(1):lower()
		if lineType == 'newmtl' then
			local mtlname = assert(words[1])
			group = self:makeOrFindGroup(mtlname, mesh, true)
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
			assert(group)
			group.Ka = wordsToColor(words)
		elseif lineType == 'kd' then	-- diffuse color
			assert(group)
			group.Kd = wordsToColor(words)
		elseif lineType == 'ks' then	-- specular color
			assert(group)
			group.Ks = wordsToColor(words)
		elseif lineType == 'ns' then	-- specular exponent
			assert(group)
			group.Ns = tonumber(words[1]) or 1
		-- 'd' = alpha
		-- 'Tr' = 1 - d = opacity
		-- 'Tf' = "transmission filter color"
		-- 'Tf xyz' = same but using CIEXYZ specs
		-- 'Tf spectral filename.rfl [factor]'
		-- 'Ni' = index of refraction aka optical density
		elseif lineType == 'map_kd' then	-- diffuse map
			assert(group)
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
				group.map_Kd = path.path
				-- TODO
				-- load textures?
				-- what if the caller isn't using GL?
				-- load images instead?
				-- just store filename and let the caller deal with it?
				group.image_Kd = Image(group.map_Kd)
--print('loaded map_Kd '..group.map_Kd..' as '..group.image_Kd.width..' x '..group.image_Kd.height..' x '..group.image_Kd.channels..' ('..group.image_Kd.format..')')
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
end)
end

function OBJLoader:makeOrFindGroup(name, mesh, inUseMtl)
	local i, group = mesh.groups:find(nil, function(g)
		return g.name == name
	end)
	if group then
		if not inUseMtl then
			if not group.triFirstIndex then
				group.triFirstIndex = #mesh.tris
				assert(not group.triCount)
				group.triCount = 0
			end
		end
		--[[
		make sure the last triangle on the group is the last recorded
		this means no doing the following:
			usemtl a
			f 1 2 3
			usemtl b
			f 4 5 6
			usemtl a
			f 7 8 9
			...
		--]]
		if group.triFirstIndex + group.triCount ~= #mesh.tris then
			print("found group ",name)
			print("but its end-of-tris is ", group.triFirstIndex+group.triCount)
			print("and the number of tris is ", #mesh.tris)
			error('here')
		end
	else
		group = {
			name = name,
			triFirstIndex = not inUseMtl and #mesh.tris or nil,
			triCount = not inUseMtl and 0 or nil,
		}
		mesh.groups:insert(group)
	end
	return group
end

-- only saves the .obj, not the .mtl
function OBJLoader:save(filename, mesh)
	local o = assert(file(filename):open'w')
	-- TODO write smooth flag, groups, etc
	if mesh.mtlFilenames then
		for _,mtlname in ipairs(mesh.mtlFilenames) do
			o:write('mtllib ', mtlname, '\n')
		end
	end

	-- keep track of all used indexes by tris
	local usedIndexes = {}
	for i=0,mesh.triIndexBuf.size-3,3 do
		local t = mesh.triIndexBuf.v + i
		local a,b,c = mesh:triVtxPos(i)
		local area = mesh.triArea(a,b,c)
		-- make sure this condition matches the tri write cond down below
		if area > 0 then
			usedIndexes[t[0]] = true
			usedIndexes[t[1]] = true
			usedIndexes[t[2]] = true
		end
	end

	local function outputUnique(symbol, field)
		-- map from the vtxs to unique indexes
		local uniquevs = table()
		-- used by tris.
		-- map from all vtxs.v[] to unique indexes
		-- keys are 0-based, values are 1-based
		local indexToUniqueV = {}
		-- maps from a key (from rounded vec3f) to uniquevs index
		-- goes a *lot* faster than the old way
		local keyToUnique = {}
		local prec = 1e-5
		for i=0,mesh.vtxs.size-1 do
			if usedIndexes[i] then
				local v = mesh.vtxs.v[i][field]
				local k = tostring(v:map(function(x) return math.round(x / prec) * prec end))
				local j = keyToUnique[k]
				if j then
					indexToUniqueV[i] = j
				else
					uniquevs:insert(v)
					keyToUnique[k] = #uniquevs
					indexToUniqueV[i] = #uniquevs
				end
			end
		end
		if self.verbose then
			print(symbol..' reduced from '..mesh.vtxs.size..' to '..#uniquevs)
		end
		for _,v in ipairs(uniquevs) do
			o:write(symbol, ' ',v.x,' ',v.y,' ',v.z,'\n')
		end
		return indexToUniqueV
	end
	local indexToUniqueV = outputUnique('v', 'pos')
	local indexToUniqueVt = outputUnique('vt', 'texcoord')
	local indexToUniqueVn = outputUnique('vn', 'normal')

	local numtriindexes = 0
	for i,group in ipairs(mesh.groups) do
		local usemtlWritten = false
		local function writeMtlName()
			if group.name == '' then return end
			if usemtlWritten then return end
			usemtlWritten = true
			o:write('usemtl ', group.name, '\n')
		end
		local lastt
		local lasttnormal
		local vis
		local function writeFaceSoFar()
			if not vis then return end
			writeMtlName()
			o:write('f ', table.mapi(vis, function(vi)
				return table{
					indexToUniqueV[vi],
					indexToUniqueVt[vi],
					indexToUniqueVn[vi],
				}:concat'/'
			end):concat' ', '\n')
			numtriindexes = numtriindexes + #vis
			vis = nil
		end
		for i=group.triFirstIndex,group.triFirstIndex+group.triCount-1 do
			assert(3*i >= 0 and 3*i < mesh.triIndexBuf.size)
			local t = mesh.triIndexBuf.v + 3*i
			local normal, area = mesh.triNormal(mesh:triVtxPos(3*i))
			if area > 0 then
				if lastt
				and t[0] == lastt[0]
				and t[1] == lastt[2]
				-- same plane
				and normal:dot(lasttnormal) > 1 - 1e-3
				and math.abs((mesh.vtxs.v[t[2]].pos - mesh.vtxs.v[lastt[2]].pos):dot(normal)) < 1e-3
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
	if self.verbose then
		print('tri indexes reduced from '..mesh.triIndexBuf.size..' to '..numtriindexes)
	end
	o:close()
end

-- TODO
-- use .mtlFilenames to determine filename?
-- or ... why store mtlFilenames at all? 
-- why not require it upon request for :save() ?
-- and if store, why not store the .obj filename too?
-- or why not combine :saveMtl and :save like i do :loadMtl and :load
function OBJLoader:saveMtl(filename, mesh)
	local o = assert(file(filename):open'w')
	for i,group in ipairs(mesh.groups) do
		if group.name ~= '' then
			o:write('newmtl ', group.name,'\n')
			for _,k in ipairs{
				'Ka', 'Kd', 'Ks', 'Ns', 'map_Kd', 
				--'map_Ks', 'map_Ns', 'map_bump', 'disp', 'decal',
			} do
				local v = group[k]
				if v then
					if vec4f:isa(v) then
						o:write(k,' ',table{v:unpack()}:concat' ','\n')
					else
						o:write(k,' ',tostring(v),'\n')
					end
				end
			end
		end
	end
	o:close()
end

return OBJLoader
