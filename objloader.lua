--  https://en.wikipedia.org/wiki/Wavefront_.obj_file
local file = require 'ext.file'
local class = require 'ext.class'
local table = require 'ext.table'
local string = require 'ext.string'
local timer = require 'ext.timer'
local math = require 'ext.math'
local matrix = require 'matrix'
local Image = require 'image'
local Mesh = require 'wavefrontobj.mesh'	-- TODO call this library 'Mesh' or something?

local mergeVertexesOnLoad = true
local mergeEdgesOnLoad = true
local unwrapUVsOnLoad = true

local function wordsToVec3(w)
	return matrix{3}:lambda(function(i)
		return tonumber(w[i]) or 0
	end)
end

-- used for colors
local function wordsToColor(w)
	-- TODO error if not 3 or 4?
	local r,g,b,a = w:mapi(function(x) return tonumber(x) end):unpack(1, 4)
	r = r or 0
	g = g or 0
	b = b or 0
	a = a or 1
	return matrix{r,g,b,a}
end


local OBJLoader = class()

function OBJLoader:load(filename)
	local mesh = Mesh()
	local vs = mesh.vs
	local vts = mesh.vts
	local vns = mesh.vns

	mesh.relpath = file(filename):getdir()
	mesh.mtlFilenames = table()

	timer('loading', function()
		
		-- map of materials
		mesh.mtllib = {}
		local curmtl = ''
		mesh.mtllib[curmtl] = {
			name = curmtl,
			-- TODO instead of redundantly storing faces,
			-- how about storing lookups into mesh.tris per-poly?
			-- and assert the tris are in a certain layout (tri fan?) for reconstructing faces?
			-- since this is only used in saving anymore.  and faceiter().
			faces = table(),
			triFirstIndex = 1,	-- index into first instance of mesh.tris for this material
			triCount = 0,	-- number of tris used in this material
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
				local usingMtl = curmtl
				local vis = table()
				local foundVT = false
				for _,vertexIndexString in ipairs(words) do
					local vertexIndexStringParts = string.split(vertexIndexString, '/')	-- may be empty string
					local vertexIndices = vertexIndexStringParts:mapi(function(x) return tonumber(x) end)	-- may be nil
					local vi, vti, vni = unpack(vertexIndices, 1, 3)
					if vti then foundVT = true end
					vis:insert{v=vi, vt=vti, vn=vni}
				end

				-- TODO hmm really?
				-- if no vt found then we can still use the Ks Kd etc from the mtl
				-- we just have to take care when drawing it not to have the texture bound
				-- (unlike the other faces in the mtl which do have vt's)
				--if not foundVT then usingMtl = '' end

				local mtl = mesh.mtllib[usingMtl]
				local facesPerPolySize = mtl.faces
				if not facesPerPolySize then
					facesPerPolySize = {}
					mtl.faces = facesPerPolySize
				end
				assert(#words >= 3, "got a bad polygon ... does .obj support lines or points?")
				local nvtx = #words
				facesPerPolySize[nvtx] = facesPerPolySize[nvtx] or table()
				facesPerPolySize[nvtx]:insert(vis)
				for i=2,nvtx-1 do
					-- store a copy of the vertex indices per triangle index
					local t = Mesh.Triangle(vis[1], vis[i], vis[i+1])
					mesh.tris:insert(t)
					-- keys:
					t.index = #mesh.tris+1	-- so far only used for debugging
					t.mtl = mtl
					if not mtl.triFirstIndex then mtl.triFirstIndex = #mesh.tris end
					mtl.triCount = #mesh.tris - mtl.triFirstIndex + 1
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
			elseif lineType == 'mtllib' then
				-- TODO this replaces %s+ with space ... so no tabs or double-spaces in filename ...
				self:loadMtl(words:concat' ', mesh)
			end
		end
	end)

	print('#tris', #mesh.tris)

-- [[ calculate bbox.  do this before merging vtxs.
	local box3 = require 'vec.box3'
	mesh.bbox = box3(-math.huge)
	for _,v in ipairs(mesh.vs) do
		mesh.bbox:stretch(v)
	end
--]]
-- TODO maybe calc bounding radius? Here or later?  That takes COM, which, for COM2/COM3 takes tris.  COM1 takes edges... should COM1 consider merged edges always?  probably... 

-- [[ merge vtxs.  TODO make this an option with specified threshold.
-- do this before detecting edges.
-- do this after bbox bounds (so I can use a %age of the bounds for the vtx dist threshold)
	if mergeVertexesOnLoad then
		timer('merging vertexes', function()
			-- ok the bbox hyp is 28, the smallest maybe valid dist is .077, and everything smalelr is 1e-6 ...
			-- that's a jump from 1/371 to 1/20,000,000
			-- so what's the smallest ratio I should allow?  maybe 1/1million?
			local bboxCornerDist = (mesh.bbox.max - mesh.bbox.min):norm()
			local vtxMergeThreshold = bboxCornerDist * 1e-6
			print('vtxMergeThreshold', vtxMergeThreshold)	
			print('before merge vtx count', #mesh.vs, 'tri count', #mesh.tris)
			for i=#mesh.vs,2,-1 do
				for j=1,i-1 do
					local dist = (mesh.vs[i] - mesh.vs[j]):norm()
		--print(dist)
					if dist < vtxMergeThreshold then
		--print('merging vtxs '..i..' and '..j)
						mesh:mergeVertex(i,j)
						break
					end
				end
			end
			print('after merge vtx count', #mesh.vs, 'tri count', #mesh.tris)
		end)
	end
--]]

-- [[ we also have to merge ..... edges .... smh.
	if mergeEdgesOnLoad then
		timer("finding edges that should've been merged by whoever made the model", function()
			--[[
			these are whatever mesh edges are partially overlapping one another.
			they are a result of a shitty artist.
			because of which, there is no guarantee with this table that each tri has 3 edges, and each edge has only 2 tris.
			instead it's a shitfest shitstorm.
			--]]
			mesh.allOverlappingEdges = {}
			for _,t in ipairs(mesh.tris) do
				t.allOverlappingEdges = table()
			end
			local function addEdge(i1, i2, j1, j2, dist, s11, s12, s21, s22, planeOrigin, planeNormal)
				-- in my loop i2 < i1, but i want it ordered lowest-first, so ... swap them
				assert(i2 < i1)
				mesh.allOverlappingEdges[i2] = mesh.allOverlappingEdges[i2] or {}
				mesh.allOverlappingEdges[i2][i1] = mesh.allOverlappingEdges[i2][i1] or {
					[1] = i2,
					[2] = i1,
					triVtxIndexes = {j2, j1},
					intervals = {{s21,s22}, {s11,s12}},
					tris = table(),
					dist = dist,
					planeOrigin = planeOrigin,
					planeNormal = planeNormal
				}
				local e = mesh.allOverlappingEdges[i2][i1]
				local t1 = mesh.tris[i1]
				local t2 = mesh.tris[i2]
				e.tris:insertUnique(t2)
				e.tris:insertUnique(t1)
				t1.allOverlappingEdges:insertUnique(e)
				t2.allOverlappingEdges:insertUnique(e)
			end
			for i1=#mesh.tris,2,-1 do
				local t1 = mesh.tris[i1]
				for j1=1,3 do
					-- t1's j1'th edge
					local v11 = mesh.vs[t1[j1].v]
					local v12 = mesh.vs[t1[j1%3+1].v]
					local n1 = v12 - v11
					local n1NormSq = n1:normSq()
					if n1NormSq  > 1e-3 then
						n1 = n1 / math.sqrt(n1NormSq)
						for i2=i1-1,1,-1 do
							local t2 = mesh.tris[i2]
							for j2=1,3 do
								local v21 = mesh.vs[t2[j2].v]
								local v22 = mesh.vs[t2[j2%3+1].v]
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
			for a,o in pairs(mesh.allOverlappingEdges) do
				for b,e in pairs(o) do
					print(
						'edges', e[1], e.triVtxIndexes[1],
						'and', e[2], e.triVtxIndexes[2],
						'align with dist', e.dist,
						'with projected intervals', table.concat(e.intervals[1], ', '),
						'and', table.concat(e.intervals[2], ', '))
				end
			end
		end)
	end
--]]

-- TODO all this per-material-group
-- should meshes have their own vtx lists?
-- or should they just index into a master list (like obj files do?)

	mesh:calcTriAux()
	mesh:findEdges()
	mesh:calcCOMs()

-- [[ calculate unique volumes / calculate any distinct pieces on them not part of the volume
	if unwrapUVsOnLoad then
		timer('unwrapping uvs', function()
			mesh:unwrapUVs()
		end)
	end
--]]

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
			mtl.faces = table()
			mtl.triFirstIndex = 1
			mtl.triCount = 0
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
				print('loaded map_Kd '..mtl.map_Kd..' as '..mtl.image_Kd.width..' x '..mtl.image_Kd.height..' x '..mtl.image_Kd.channels..' ('..mtl.image_Kd.format..')')
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

function OBJLoader:save(filename, mesh)
	local o = assert(file(filename):open'w')
	-- TODO write smooth flag, groups, etc
	for _,mtl in ipairs(mesh.mtlFilenames) do
		o:write('mtllib ', mtl, '\n')
	end
	for _,v in ipairs(mesh.vs) do
		o:write('v ', table.concat(v, ' '), '\n')
	end
	for _,vt in ipairs(mesh.vts) do
		o:write('vt ', table.concat(vt, ' '), '\n')
	end
	for _,vn in ipairs(mesh.vns) do
		o:write('vn ', table.concat(vn, ' '), '\n')
	end
	local mtlnames = table.keys(mesh.mtllib):sort()
	assert(mtlnames[1] == '')	-- should always be there 
	for _,mtlname in ipairs(mtlnames) do
		local mtl = mesh.mtllib[mtlname]
		if mtlname ~= '' then
			o:write('usemtl ', mtlname, '\n')
		end
		local fs = mtl.faces
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

return OBJLoader
