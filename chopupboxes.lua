#!/usr/bin/env luajit
local table = require 'ext.table'
local file = require 'ext.file'
local math = require 'ext.math'
local timer = require 'ext.timer'
local range = require 'ext.range'
local vec3i = require 'vec-ffi.vec3i'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'
local Mesh = require 'mesh'

local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")

local loader = require 'mesh.objloader'()
local mesh = loader:load(infn)
mesh:calcBBox()
print('bbox', mesh.bbox)
for i=1,3 do
	mesh.bbox.min[i] = math.floor(mesh.bbox.min[i])
	mesh.bbox.max[i] = math.ceil(mesh.bbox.max[i])
end
print('bbox after rounding', mesh.bbox)
mesh:breakTriangles()

-- 1) bin all triangles
local comEps = .1
local normalStepEps = .01
local trisForBox = {}
local mini = vec3i(999,999,999)
local maxi = -mini
local tboxes = table()
for i=0,mesh.triIndexBuf.size-3,3 do
	local a,b,c = mesh:triVtxPos(i)
	local com = mesh.triCOM(a,b,c)
	local normal, area = mesh.triNormal(a,b,c)

	-- calc tri bounds
	local tmini = vec3i(999,999,999)
	local tmaxi = -mini

	for j=0,2 do
		local v = mesh.vtxs.v[mesh.triIndexBuf.v[i+j]].pos
		v = v * (1 - comEps) + com * comEps - normal * normalStepEps
		local iv = vec3i()
		for k=0,2 do
			-- pull slightly towards tri com and against normal
			--[[ just using the vtx and pushing/pulling isn't working
			-- binning too coarse or too fine
			mini.s[k] = math.min(mini.s[k], math.floor(v.s[k] + eps))
			maxi.s[k] = math.max(maxi.s[k], math.floor(v.s[k] - eps))
			tmini.s[k] = math.min(tmini.s[k], math.floor(v.s[k] + eps))
			tmaxi.s[k] = math.max(tmaxi.s[k], math.floor(v.s[k] - eps))
			--]]
			-- [[ so instead, push towards tri com and push back from the fwd-facing normal
			iv.s[k] = math.floor(v.s[k])
			mini.s[k] = math.min(mini.s[k], iv.s[k])
			maxi.s[k] = math.max(maxi.s[k], iv.s[k])
			tmini.s[k] = math.min(tmini.s[k], iv.s[k])
			tmaxi.s[k] = math.max(tmaxi.s[k], iv.s[k])
			--]]
		end
		tboxes:insert{tmini, tmaxi}

		for ix=tmini.x,tmaxi.x do
			for iy=tmini.y,tmaxi.y do
				for iz=tmini.z,tmaxi.z do
					trisForBox[ix] = trisForBox[ix] or {}
					trisForBox[ix][iy] = trisForBox[ix][iy] or {}
					trisForBox[ix][iy][iz] = trisForBox[ix][iy][iz] or {}
					trisForBox[ix][iy][iz][i] = true
				end
			end
		end
	end
end
print('# bounding boxes', #tboxes)
--print('boxes', require 'ext.tolua'(trisForBox))

local function touches(b1,b2)
	return b1[1].x <= b2[2].x
		and b1[2].x >= b2[1].x
		and b1[1].y <= b2[2].y
		and b1[2].y >= b2[1].y
		and b1[1].z <= b2[2].z
		and b1[2].z >= b2[1].z
end

-- 2) group bins where triangles span multiple bins
for i=#tboxes,2,-1 do
	local k = i	-- holder of original i'th pieces
	for j=i-1,1,-1 do
		if touches(tboxes[i], tboxes[j]) then
			-- move from k to j
			local dstx = tboxes[j][1].x
			local dsty = tboxes[j][1].y
			local dstz = tboxes[j][1].z
			print('merging', k, 'into', j)
			for _,srcbox in ipairs{tboxes[k], tboxes[j]} do
				for ix=srcbox[1].x,srcbox[2].x do
					for iy=srcbox[1].y,srcbox[2].y do
						for iz=srcbox[1].z,srcbox[2].z do
							if not (ix == dstx and iy == dsty and iz == dstz) then
								trisForBox[dstx] = trisForBox[dstx] or {}
								trisForBox[dstx][dsty] = trisForBox[dstx][dsty] or {}
								trisForBox[dstx][dsty][dstz] = trisForBox[dstx][dsty][dstz] or {}
								trisForBox[dstx][dsty][dstz] = table(trisForBox[dstx][dsty][dstz], trisForBox[ix][iy][iz]):setmetatable(nil)
								trisForBox[ix][iy][iz] = nil
								--[[
								if not next(trisForBox[ix][iy]) then
									trisForBox[ix][iy] = nil
									if not next(trisForBox[ix]) then
										trisForBox[ix] = nil
									end
								end
								--]]
							end
						end
					end
				end
			end
			-- j is the new k / dst tbox with this cluster's nodes
			k = j
			-- don't break out of the loop
			-- so the pieces go thru the cluster and collect at the smallest index
		end
	end
	if k ~= i then	-- moved?
		tboxes:remove(i)
	end
end
for i=#tboxes,1,-1 do
	local tbox = tboxes[i]
	local empty = true
	for ix=tbox[1].x,tbox[2].x do
		for iy=tbox[1].y,tbox[2].y do
			for iz=tbox[1].z,tbox[2].z do
				if trisForBox[ix]
				and trisForBox[ix][iy]
				and trisForBox[ix][iy][iz]
				then
					if next(trisForBox[ix][iy][iz]) then
						empty = false
					else
						trisForBox[ix][iy][iz]= nil
						if not next(trisForBox[ix][iy]) then
							trisForBox[ix][iy] = nil
							if not next(trisForBox[ix]) then
								trisForBox[ix] = nil
							end
						end
					end
				end
			end
		end
	end
	if empty then
		print('erasing', i)
		tboxes:remove(i)
	end
end

tboxes:sort(function(a,b)
	if a[1].z < b[1].z then return true end
	if a[1].z > b[1].z then return false end
	if a[1].y < b[1].y then return true end
	if a[1].y > b[1].y then return false end
	return a[1].x > b[1].x
end)
local numBinnedTris = 0
for _,b in ipairs(tboxes) do
	local tris = table.keys(trisForBox[b[1].x][b[1].y][b[1].z]):sort()
	numBinnedTris = numBinnedTris + #tris
	print(b[1], b[2], tris:concat', ')
end
print('# clusters left', #tboxes)
print('# binned tris', numBinnedTris)
print('# orig tris', mesh.triIndexBuf.size/3)
print('ibounds', mini, maxi)

-- now convert tboxes tris into unique materials
-- and export
mesh.groups = table()
mesh.triIndexBuf:resize(0)
local origMtlFn = mesh.mtlFilenames[1]
mesh.mtlFilenames = {(outfn:gsub('%.obj$', '.mtl'))}
for i,tbox in ipairs(tboxes) do
	local x,y,z = tbox[1]:unpack()
	local tris = table.keys(trisForBox[x][y][z]):sort()
	local groupname = 'm'..i
	mesh.groups:insert{
		name = groupname,
		triFirstIndex = mesh.triIndexBuf.size / 3,
		triCount = #tris,
		Kd = vec4f(
			tonumber(x - mini.x) / tonumber(maxi.x - mini.x),
			tonumber(y - mini.y) / tonumber(maxi.y - mini.y),
			tonumber(z - mini.z) / tonumber(maxi.z - mini.z),
			1),
	}
	for _,j in ipairs(tris) do
		for k=0,2 do
			mesh.triIndexBuf:push_back(j+k)
			-- since our tris are already broken up, we can manipulate the vtx without it messing up any other tris
		end
	end
end

-- now comes a new trick ...
-- filling in missing faces on a mesh.
-- tough problem
-- easy solution: use planes and clip
-- harder solution ... extrude.
file'blocks':mkdir()
assert(file'blocks':isdir())

for tboxIndex,tbox in ipairs(tboxes) do
	local dstfn = 'blocks/'..tboxIndex..'.obj'
	timer('saving '..dstfn, function()
		local m = Mesh()
		local x,y,z = tbox[1]:unpack()
		print('at',x,y,z)
		local tris = table.keys(trisForBox[x][y][z]):sort()
		for _,i in ipairs(tris) do
			for j=0,2 do
				m.triIndexBuf:push_back(m.vtxs.size)
				m.vtxs:push_back(mesh.vtxs.v[i+j])
				local v = m.vtxs:back()
				v.pos = v.pos - vec3f(x,y,z)
			end
		end
		assert(m.triIndexBuf.size == m.vtxs.size)
		for i=0,m.triIndexBuf.size-1 do
			assert(m.triIndexBuf.v[i] >= 0 and m.triIndexBuf.v[i] < m.vtxs.size)
		end
		m.tris = range(m.triIndexBuf.size/3):mapi(function(i)
			return {
				{v=i, vt=i, vn=i},
				{v=i, vt=i, vn=i},
				{v=i, vt=i, vn=i},
			}	-- findEdges stores stuff in here ... but idk
		end)

		-- [[ ok here, per-side, seal off the mesh.

		-- find all open edges, and find what sides they touch ...
		-- TODO what about two triangles, completely overlapping, but varying texcoords?
		-- because that's what's happening on block #4 ...
		--m:mergeMatchingVertexes()

		--[==[ this isn't help anthing
		assert(#m.tris*3 == m.triIndexBuf.size)
		local prec = 1e-5
		local function makekey(i)
			return vec3i(range(0,2):mapi(function(k) return m.triIndexBuf.v[i+k] end):sort():unpack())
			--[=[
			return range(0,2):mapi(function(k)
				return m.vtxs.v[m.triIndexBuf.v[i+k]].pos
			end):sort(function(a,b)
				if a.x == b.x then
					if a.y == b.y then
						return a.z < b.z
					end
					return a.y < b.y
				end
				return a.x < b.x
			end):mapi(function(v)
				return tostring(v:map(function(x)
					if math.abs(x) < prec then return 0 end	-- avoid -0's
					return math.round(x / prec) * prec
				end))
			end):concat' '
			--]=]
		end
		for i=m.triIndexBuf.size-3,3,-3 do
			local ki = makekey(i)
print(ki)
			for j=i-3,0,-3 do
				local kj = makekey(j)
--print(ki,kj)
				if ki == kj then
					error("found identical triangles")
				end
			end
		end
		--]==]

		-- TODO gotta find edges based on vtx comparing pos
		-- not based on vtx index
		m:findEdges(function(i)
			for j=0,i-1 do
				if (m.vtxs.v[j].pos - m.vtxs.v[i].pos):norm() < 1e-6 then
					return j
				end
			end
			return i
		end)

		local border = table()
		local totalEdges = 0
		for a,o in pairs(m.edges) do
			for b,e in pairs(o) do
				if #e.tris == 1 then
					border:insert(e)
				end
				totalEdges = totalEdges + 1
			end
		end
print('edges total', totalEdges, 'border', #border)

		--[=[
		TODO turn this into a mesh function: find boundary edges
		do	-- now put in loops
			local all = table(border)
			local loops = table()
			local lines = table()
			while #all > 0 do
				local loop = table()
				local last = all:remove(1)
--print('first edge', last[1], last[2])
				loop:insert{v=1, e=last}
				local lastvi = 2
				while true do
					local found
					for i=1,#all do
						local o = all[i]
--print('checking edge', o[1], o[2])
						if o[1] == last[lastvi] then
							last = o
							lastvi = 2
							loop:insert{v=3-lastvi, e=o}
							all:remove(i)
							found = true
--print('adding edge', last[1], last[2])
							break
						elseif o[2] == last[lastvi] then
							last = o
							lastvi = 1
							loop:insert{v=3-lastvi, e=o}
							all:remove(i)
							found = true
--print('adding edge', last[1], last[2])
							break
						end
					end
					if not found then
--print('found no more edges, adding to lines')
						lines:insert(loop)
						break
					else
						if last[lastvi] == loop[1].e[loop[1].v] then
--print('reached beginning, adding to loops')
							loops:insert(loop)
							break
						end
					end
				end
			end
print('#loops', #loops)
print('#lines', #lines)

			-- no boundary edges that aren't loops
			-- lines?  how to fix those?
			if #lines > 0 then error("can't fix stupid") end
			-- luckily I never have to find out (yet)

			-- which blocks have more than one loop?
			-- block 4 at (10, 1, 0)
			-- block 5 at (9, 1, 0)
			-- block 6 at (8, 1, 0)
			-- block 7 at (7, 1, 0)
			-- block 8 at (6, 1, 0)
			-- block 9 at (5, 1, 0)
			-- block 10 at (4, 1, 0)
			-- block 11 at (3, 1, 0)
			-- block 12 at (2, 1, 0)
			-- block 13 at (1, 1, 0)
			-- block 14 at (10, 2, 0)
			-- and still going ...
			if #loops > 1 then
				print("!!!!!!!!!! failed to find just one loop for shape "..dstfn..' !!!!!!!!!!')
			end

			local function getIndexForLoopChain(l)
				local i = l.e[l.v]-1
				assert(i >= 0 and i < m.vtxs.size)
				return i
			end

			for i,loop in ipairs(loops) do
print('loop '..loop:mapi(function(l) return getIndexForLoopChain(l) end):concat', ')
				--[[ determine if the loop is planar (and determine its normal)
				for j=1,#loop-1 do
					local ia = getIndexForLoopChain(loop[j])
					local a = m.vtxs.v[ia].pos
					local ib = getIndexForLoopChain(loop[j%#loop+1])
					local b = m.vtxs.v[ib].pos
					local ic = getIndexForLoopChain(loop[(j+1)%#loop+1])
					local c = m.vtxs.v[ic].pos
					local n = (c - b):cross(b - a)
					print(n)
				end
				--]]
				-- [[ just add the tris as-is
-- TODO how to determine loop order ...
-- probably based on normal of opposing edges triangles
if loop[1].e.tris[1][1].v == loop[1].e[1] then
	loop = loop:reverse()
end
				for j=2,#loop-1 do
					local ia = getIndexForLoopChain(loop[1])
					m.triIndexBuf:push_back(ia)
					local ib = getIndexForLoopChain(loop[j])
					m.triIndexBuf:push_back(ib)
					local ic = getIndexForLoopChain(loop[j+1])
					m.triIndexBuf:push_back(ic)
				end
				--]]
				assert(#loop >= 3)
			end
		end
		--]=]
		--]]

		-- [=[ look at vtxs that are along the bbox
		-- TODO combine this with the edge border loops
		m:calcBBox()
		print('bbox', m.bbox)
		for side=0,2 do
			for pm=0,1 do
				local vs = range(0,m.vtxs.size-1):filter(function(i)
					return m.vtxs.v[i].pos.s[side] == m.bbox[({'min','max'})[pm+1]][side+1]
				end)

				-- find what border edges are on this side
				local es = border:mapi(function(e,i,t)
					for j=1,2 do
						if m.vtxs.v[e[j]-1].pos.s[side] ~= m.bbox[({'min','max'})[pm+1]][side+1] then return end
					end
					return e, #t+1
				end)

				print('side', side, '+-', pm, '#vs', #vs, '#es', #es)
			end
		end
		--]=]

		-- [[ fix up the mtl part of Mesh
		m.mtlFilenames = {origMtlFn}
		m.groups = table{
			{
				name = 'm',
				triFirstIndex = 0,
				triCount = m.triIndexBuf.size/3,
			},
		}
		--]]
		loader:save(dstfn, m)
	end)
	break
end

loader:save(outfn, mesh)
loader:saveMtl(mesh.mtlFilenames[1], mesh)
