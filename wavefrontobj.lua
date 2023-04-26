--  https://en.wikipedia.org/wiki/Wavefront_.obj_file
local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local string = require 'ext.string'
local file = require 'ext.file'
local math = require 'ext.math'
local timer = require 'ext.timer'
local vec2 = require 'vec.vec2'
local vec3 = require 'vec.vec3'
local vec4 = require 'vec.vec4'
local quat = require 'vec.quat'
local matrix = require 'matrix'
local vector = require 'ffi.cpp.vector'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local Image = require 'image'

ffi.cdef[[
typedef struct {
	vec3f_t pos;
	vec3f_t normal;		//loaded normal
	vec3f_t normal2;	//generated normal ... because i want the viewer to toggle between the two
	vec2f_t texCoord;
	
	// per-triangle stats (duplicated 3x per-vertex)
	float area;
	vec3f_t com;		//com of tri containing this vertex.  only good for un-indexed drawing.
} obj_vertex_t;
]]

local function triArea(a,b,c)
	local ab = b - a
	local ac = c - a
	return .5 * ab:cross(ac):length()
end

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

local function wordsToNumbers(w)
	return w:mapi(function(x) return tonumber(x) end):unpack(1, table.maxn(w))
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
	local r,g,b,a = w:mapi(function(x) return tonumber(x) end):unpack(1, 4)
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
		self.tris = table() -- triangulation of all faces
		
		-- map of materials
		self.mtllib = {}
		local curmtl = ''
		self.mtllib[curmtl] = {
			name = curmtl,
			faces = table(),
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

				local facesPerPolySize = self.mtllib[usingMtl].faces
				if not facesPerPolySize then
					facesPerPolySize = {}
					self.mtllib[usingMtl].faces = facesPerPolySize
				end
				assert(#words >= 3, "got a bad polygon ... does .obj support lines or points?")
				local nvtx = #words
				facesPerPolySize[nvtx] = facesPerPolySize[nvtx] or table()
				facesPerPolySize[nvtx]:insert(vis)
				for i=2,nvtx-1 do
					-- store a copy of the vertex indices per triangle index
					self.tris:insert{
						index = #self.tris+1,
						table(vis[1]):setmetatable(nil),
						table(vis[i]):setmetatable(nil),
						table(vis[i+1]):setmetatable(nil),
					}
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
				self:loadMtl(words:concat' ')
			end
		end
		-- could've done this up front...
		self.vs = vs
		self.vts = vts
		self.vns = vns
	end)

-- TODO all this per-material-group
-- should meshes have their own vtx lists?
-- or should they just index into a master list (like obj files do?)

	-- store tri area
	for _,t in ipairs(self.tris) do
		t.area = triArea(self.vs[t[1].v], self.vs[t[2].v], self.vs[t[3].v])
	end

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
				length = (self.vs[a] - self.vs[b]):length(),
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
-- [[ calculate bbox
	local vec3huge = vec3f(math.huge, math.huge, math.huge)
	self.bbox = {
		min = vec3f(vec3huge),
		max = -vec3huge,
	}
	for _,v in ipairs(self.vs) do
		for i=0,2 do
			self.bbox.min.s[i] = math.min(self.bbox.min.s[i], v[i+1])
			self.bbox.max.s[i] = math.max(self.bbox.max.s[i], v[i+1])
		end
	end
--]]
-- TODO maybe calc bounding radius?

-- [=[ calculate unique volumes / calculate any distinct pieces on them not part of the volume
-- TODO put this all in its own function or its own app
	local numSharpEdges = 0
	for a,other in pairs(self.edges) do
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

	timer('unwrapping uvs', function()
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
			local t1,t2 = table.unpack(e.tris)
			if t2 == t then
				t1, t2 = t2, t1
			end
			assert(t1 == t)
			return t2, t1
		end
		local function calcUVBasis(t, tsrc, esrc)
			-- t[1] is our origin
			-- t[1]->t[2] is our x axis with unit length
			local v = matrix{3,3}:lambda(function(i,j) return self.vs[t[i].v][j] end)
	--print('v\n'..v)					
			local d1 = v[2] - v[1]
			local d2 = v[3] - v[2]
			local n = d1:cross(d2)
			local nlen = n:norm()
	--print('|d1 x d2| = '..nlen)
			if nlen < 1e-9 then
				return true
				-- can't fold this because i'ts not a triangle ... it's a line
			end
			n = n / nlen
	--print('n = '..n)
			t.normal = matrix(n)
		
			if not tsrc then	-- first basis
				t.uvorigin2D = matrix{0,0}
				t.uvorigin3D = matrix(v[1])
	--print('uv2D = '..t.uvorigin2D)
	--print('uv3D = '..t.uvorigin3D)
				local ex = d1:normalize()
	--print('ex = '..ex)			
				-- tangent space.  store as row vectors i.e. transpose, hence the T
				t.uvbasisT = matrix{
					ex,
					n:cross(ex):normalize(),
					n,
				}
	--print('ey = '..t.uvbasisT[2])			
			else
			
	--[[
	tsrc.v3      tsrc.v2
		   *-------* t.v2
		   |   ___/|
		   |__/    |
	tsrc.v1*-------*
		  t.v3   t.v1
	--]]
	--print('folding from', tsrc.index, 'to', t.index)
				local i11 = findLocalIndex(tsrc, esrc[1])	-- where in tsrc is the edge's first?
				local i12 = findLocalIndex(tsrc, esrc[2])	-- where in tsrc is the edge's second?
				local i21 = findLocalIndex(t, esrc[1])	-- where in t is the edge's first?
				local i22 = findLocalIndex(t, esrc[2])	-- where in t is the edge's second?
	--print('edge local vtx indexes: tsrc', i11, i12, 't', i21, i22)					
				assert(i11 and i12 and i21 and i22)
				assert(tsrc[i11].v == t[i21].v)	-- esrc[1] matches between tsrc and t
				assert(tsrc[i12].v == t[i22].v)	-- esrc[2] matches between tsrc and t
				-- tables are identical
				assert(tsrc[i11].v == esrc[1])
				assert(tsrc[i12].v == esrc[2])
				assert(t[i21].v == esrc[1])
				assert(t[i22].v == esrc[2])

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
		
				--[[ first tri basis
				local ex = d1:normalize()
				t.uvbasisT = matrix{
					ex,
					n:cross(ex):normalize(),
					n,
				}
				--]]
				-- [[ subsequent tri basis should be constructed from rotating the prev tri basis
				-- find the rotation from normal 1 to normal 2
				-- that'll just be the matrix formed from n1 and n2's basis ...
				local q = quat():vectorRotate(tsrc.normal, t.normal)
				t.uvbasisT = matrix{
					q:rotate(tsrc.uvbasisT[1]),
					q:rotate(tsrc.uvbasisT[2]),
					n,
				}
	--print('|ez-n| = '..matrix(q:rotate(tsrc.uvbasisT[3]) - n):norm())
				--]]
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
				if not math.isfinite(t[i].uv[1]) or not math.isfinite(t[i].uv[2]) then
					error("here")
				end
			end
		end
		local notDoneYet = table(self.tris)
		while #notDoneYet > 0 do
			-- TODO heuristic of picking best starting edge
			print('starting unwrapping process with '..#notDoneYet..' left')
			local t = notDoneYet:remove(1)
			local done = table()
			local todo = table{t}
			
			-- TODO maybe instead I should be tracking all live edges?
			-- so process the first tri as the starting point
			-- then add its edges into the 'todo' list

			while #todo > 0 do
				local t, tsrc, e
				if #todo == 1 and #done == 0 then
					-- first iteration
					t = todo:remove(1)
					assert(not tsrc)
					assert(not e)
				else
					-- pick best edge between any triangle in 'done' and any in 'todo'
					local edgesToCheck = table()
					for _,t in ipairs(todo) do
						for _,e in ipairs(t.edges) do
							local t2 = getEdgeOppositeTri(e, t)
							if done:find(t2) then
								edgesToCheck:insert{tri=t, edge=e, prevtri=t2}
							end
						end
					end
					
					-- assert from prevoius iteration that the first is the best
					-- heuristic for picking best continuing edge
					-- sort last instead of first, so first iteration and first entry is removed, so I can guarantee that all entries have .prevtri and .edge
					edgesToCheck:sort(function(a,b)
						local ta, tb = a.tri, b.tri
						local ea, eb = a.edge, b.edge
						-- [[ prioritize longest edge ... cube makes a long straight shape with one bend.
						-- looks best for cone.  just does two solid pieces for the base and sides
						-- looks best for cube.  does the cubemap t.
						return ea.length > eb.length
						--]]
						--[[ prioritize shortest edge ... cube makes a zigzag
						return ea.length < eb.length
						--]]
						--[[ prioritize biggest area
						return ta.area > tb.area
						--]]
						--[[ prioritize smallest area
						return ta.area > tb.area
						--]]
					end)
					local check = edgesToCheck[1]
					t, e, tsrc = check.tri, check.edge, check.prevtri
					assert(t)
					assert(e)
					assert(tsrc)
					todo:removeObject(t)
				end
				-- calc the basis by rotating it around the edge
				assert((tsrc == nil) == (e == nil))
				local foundLine = calcUVBasis(t, tsrc, e)
				done:insert(t)
				if not foundLine then
					assert(t[1].uv and t[2].uv and t[3].uv)
					-- insert neighbors into a to-be-calcd list
	--print('tri', t.index)
					for _,e in ipairs(t.edges) do
	--print('edge length', e.length)
						-- for all edges in the t, go to the other faces matching.
						-- well, if there's more than 2 faces shared by an edge, that's a first hint something's wrong.
						if #e.tris == 2 then
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
		end
	end)
end

function WavefrontOBJ:loadMtl(filename)
	self.mtlFilenames:insert(filename)
	local mtl
	filename = file(self.relpath)(filename).path
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
			local path = file(self.relpath)(localpath)
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
			local mtl = self.mtllib[mtlname]
			if mtl then coroutine.yield(mtl, mtlname) end
		else
			for mtlname, mtl in pairs(self.mtllib) do
				coroutine.yield(mtl, mtlname)
			end
		end
	end)
end

-- yields with each face in a particular material or in all materials
function WavefrontOBJ:faceiter(mtlname)
	return coroutine.wrap(function()
		for mtl in self:mtliter(mtlname) do
			local facesPerPolySize = assert(mtl.faces)
			-- order not guaranteed:
			--for polySize,faces in pairs(facesPerPolySize) do
			-- order guaranteed, but fails for no-triangles
			--for polySize=3,table.maxn(facesPerPolySize) do
			-- involves a sort so ..
			for _,polySize in ipairs(table.keys(facesPerPolySize):sort()) do
				local faces = facesPerPolySize[polySize]
				for _,vis in ipairs(faces) do
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
		local area = triArea(a, b, c)
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
		-- volume of parallelogram with vertices at 0, a, b, c
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
	for _,mtlname in ipairs(table.keys(self.mtllib):sort()) do
		local mtl = self.mtllib[mtlname]
		o:write('usemtl ', mtlname, '\n')
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


-- all the draw functionality is tied tightly with view.lua so ... 
-- idk if i should move it from one or the other


-- upon ctor the images are loaded (in case the caller isn't using GL)
-- so upon first draw - or upon manual call - load the gl textures
function WavefrontOBJ:loadGL(shader)
	local gl = require 'gl'
	local glreport = require 'gl.report'
	local GLTex2D = require 'gl.tex2d'
	local GLArrayBuffer = require 'gl.arraybuffer'
	local GLAttribute = require 'gl.attribute'
	local GLVertexArray = require 'gl.vertexarray'
	
	-- load textures
	for mtlname, mtl in pairs(self.mtllib) do
		if mtl.image_Kd and not mtl.tex_Kd then
			mtl.tex_Kd = GLTex2D{
				image = mtl.image_Kd,
				minFilter = gl.GL_NEAREST,
				magFilter = gl.GL_LINEAR,
			}
		end
	end

	-- now for performance I can either store everything in a packed array
	-- or I can put unique index sets' data in a packed array and store the unique # in an index array (more complex but more space efficient)
	for mtlname, mtl in pairs(self.mtllib) do
		if not mtl.vtxCPUBuf then
			-- count total number of triangles
			-- TODO save this #?
			-- TODO save the triangulation?
			local i = 0
			for a,b,c in self:triiter(mtlname) do
				i = i + 3
			end
			
			--[[ save face normals and face area?
			for polySize,faces in pairs(self.mtllib[mtlname].faces) do
				for _,face in ipairs(faces) do
					for j=2,polySize-1 do
						local a = face[1]
						local b = face[j]
						local c = face[j+1]
					end
				end
			end
			--]]

			-- calculate vertex normals
			local vtxnormals = {}
			for a,b,c in self:triiter(mtlname) do
				local va = self.vs[a.v]
				local vb = self.vs[b.v]
				local vc = self.vs[c.v]
				local normal = (vb - va):cross(vc - vb):normalize()
				for _,vi in ipairs{a,b,c} do
					vtxnormals[vi.v] = (vtxnormals[vi.v] or vec3()) + normal
				end
			end
			for _,k in ipairs(table.keys(vtxnormals)) do
				vtxnormals[k] = vtxnormals[k]:normalize()
			end
			
			local vtxCPUBuf = vector('obj_vertex_t', i)
			i = 0
			for a,b,c in self:triiter(mtlname) do
				local va = self.vs[a.v]
				local vb = self.vs[b.v]
				local vc = self.vs[c.v]
				local com = (va + vb + vc) / 3
				local area = triArea(va, vb, vc)
				for _,vi in ipairs{a,b,c} do
					local v = vtxCPUBuf.v + i
					v.pos:set(self.vs[vi.v]:unpack())
					if vi.vt then
						if vi.vt < 1 or vi.vt > #self.vts then
							print("found an oob vt "..vi.vt)
						else
							v.texCoord:set(self.vts[vi.vt]:unpack())
						end
					end
					if vi.vn then
						if vi.vn < 1 or vi.vn > #self.vns then
							print("found an oob fn "..vi.vn)
						else
							v.normal:set(self.vns[vi.vn]:unpack())
						end
					end
					v.normal2:set(vtxnormals[vi.v]:unpack())
					v.area = area
					v.com:set(com:unpack())
					i = i + 1
				end
			end
			mtl.vtxCPUBuf = vtxCPUBuf
		
			-- [=[
			mtl.vtxBuf = GLArrayBuffer{
				size = mtl.vtxCPUBuf.size * ffi.sizeof'obj_vertex_t',
				data = mtl.vtxCPUBuf.v,
				usage = gl.GL_STATIC_DRAW,
			}
			assert(glreport'here')

			mtl.vtxAttrs = {}
			for _,info in ipairs{
				{name='pos', size=3},
				{name='texCoord', size=2},
				{name='normal', size=3},
				{name='normal2', size=3},
				{name='com', size=3},
			} do
				local srcAttr = shader.attrs[info.name]
				if srcAttr then
					mtl.vtxAttrs[info.name] = GLAttribute{
						buffer = mtl.vtxBuf,
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
			mtl.vao = GLVertexArray{
				program = shader,
				attrs = mtl.vtxAttrs,
			}
			shader:setAttrs(mtl.vtxAttrs)
			shader:useNone()
			assert(glreport'here')
			--]=]
		end
	end
end

function WavefrontOBJ:draw(args)
	local gl = require 'gl'
	
	self:loadGL()	-- load if not loaded
	local curtex
	for mtlname, mtl in pairs(self.mtllib) do
		local fs = mtl.faces
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
		gl.glTexCoordPointer(2, gl.GL_FLOAT, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].texCoord.s)
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
		gl.glVertexAttribPointer(args.shader.attrs.texCoord.loc, 2, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'obj_vertex_t', mtl.vtxCPUBuf.v[0].texCoord.s)
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
		mtl.vao:use()
		gl.glDrawArrays(gl.GL_TRIANGLES, 0, mtl.vtxCPUBuf.size)
		mtl.vao:useNone()
		--]]
		if args.endMtl then args.endMtl(mtl) end
	end
	if curtex then
		curtex:unbind()
		curtex:disable()
	end
	require 'gl.report''here'
end

-- make sure my edges match my faces
function WavefrontOBJ:drawEdges()
	local gl = require 'gl'
	gl.glColor3f(1,1,1)
	gl.glBegin(gl.GL_LINES)
	for a,other in pairs(self.edges) do
		for b,edge in pairs(other) do
			gl.glVertex3f(self.vs[a]:unpack())
			gl.glVertex3f(self.vs[b]:unpack())
		end
	end
	gl.glEnd()
end

function WavefrontOBJ:drawNormals(useNormal2)
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for mtlname,mtl in pairs(self.mtllib) do
		if mtl.vtxCPUBuf then	-- default mtl '' can be empty...
			for i=0,mtl.vtxCPUBuf.size-1,3 do
				local v = mtl.vtxCPUBuf.v[i]
				gl.glVertex3f(v.com:unpack())
				if not useNormal2 then
					gl.glVertex3f((v.com + v.normal):unpack())
				else
					gl.glVertex3f((v.com + v.normal2):unpack())
				end
			end
		end
	end
	gl.glEnd()
end

function WavefrontOBJ:drawUVs(_3D)
	local gl = require 'gl'
	local GLTex2D = require 'gl.tex2d'
	self.uvMap = self.uvMap or GLTex2D{
		image = Image(64, 64, 3, 'unsigned char', function(u,v)
			return (u+.5)/64*255, (v+.5)/64*255, 127
		end),
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_LINEAR,
		wrap = {s = gl.GL_REPEAT, t = gl.GL_REPEAT},
	}
	gl.glColor3f(1,1,1)
	for mode=0,1 do
		if mode == 0 then
			self.uvMap:enable()
			self.uvMap:bind()
		else
			gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_LINE)
		end
		gl.glBegin(gl.GL_TRIANGLES)
		for _,t in ipairs(self.tris) do
			for _,tv in ipairs(t) do
				uv = tv.uv or {0,0}
				gl.glTexCoord2f(uv[1], uv[2])
				if _3D then
					gl.glVertex3f(self.vs[tv.v]:unpack())
				else
					gl.glVertex2f(uv[1], uv[2])
				end
			end
		end
		gl.glEnd()
		if mode == 1 then
			self.uvMap:unbind()
			self.uvMap:disable()
		else
			gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_FILL)
		end
	end
end

return WavefrontOBJ
