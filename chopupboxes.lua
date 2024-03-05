#!/usr/bin/env luajit
local table = require 'ext.table'
local path = require 'ext.path'
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
for i=0,2 do
	mesh.bbox.min.s[i] = math.floor(mesh.bbox.min.s[i])
	mesh.bbox.max.s[i] = math.ceil(mesh.bbox.max.s[i])
end
print('bbox after rounding', mesh.bbox)
mesh:breakAllVertexes()

-- 1) bin all triangles
local comEps = .1
local normalStepEps = .01
local trisForBox = {}
local mini = vec3i(999,999,999)
local maxi = -mini
local tboxes = table()
for i=0,#mesh.triIndexes-3,3 do
	local t = mesh.tris[i/3+1]
	local a,b,c = t:vtxPos(mesh)
	local com = t.com
	local normal, area = t.normal, t.area

	-- calc tri bounds
	local tmini = vec3i(999,999,999)
	local tmaxi = -mini

	for j=0,2 do
		local v = mesh.vtxs.v[mesh.triIndexes.v[i+j]].pos
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
	if a[1].z ~= b[1].z then return a[1].z < b[1].z end
	if a[1].y ~= b[1].y then return a[1].y < b[1].y end
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
print('# orig tris', #mesh.triIndexes/3)
print('ibounds', mini, maxi)

-- now convert tboxes tris into unique materials
-- and export
mesh.groups = table()
mesh.triIndexes:resize(0)
local origMtlFn = mesh.mtlFilenames[1]
mesh.mtlFilenames = {(outfn:gsub('%.obj$', '.mtl'))}
for i,tbox in ipairs(tboxes) do
	local x,y,z = tbox[1]:unpack()
	local tris = table.keys(trisForBox[x][y][z]):sort()
	local groupname = 'm'..i
	mesh.groups:insert{
		name = groupname,
		triFirstIndex = #mesh.triIndexes / 3,
		triCount = #tris,
		Kd = vec4f(
			tonumber(x - mini.x) / tonumber(maxi.x - mini.x),
			tonumber(y - mini.y) / tonumber(maxi.y - mini.y),
			tonumber(z - mini.z) / tonumber(maxi.z - mini.z),
			1),
	}
	for _,j in ipairs(tris) do
		for k=0,2 do
			mesh.triIndexes:push_back(j+k)
			-- since our tris are already broken up, we can manipulate the vtx without it messing up any other tris
		end
	end
end
mesh:rebuildTris()

loader:save(outfn, mesh)
loader:saveMtl(mesh.mtlFilenames[1], mesh)


-- now comes a new trick ...
-- filling in missing faces on a mesh.
-- tough problem
-- easy solution: use planes and clip
-- harder solution ... extrude.
path'blocks-v1':mkdir()
assert(path'blocks-v1':isdir())

for tboxIndex,tbox in ipairs(tboxes) do
	local dstfn = 'blocks-v1/'..tboxIndex..'.obj'
	timer('saving '..dstfn, function()
		local m = Mesh()
		local x,y,z = tbox[1]:unpack()
		print('at',x,y,z)
		local tris = table.keys(trisForBox[x][y][z]):sort()
		for _,i in ipairs(tris) do
			for j=0,2 do
				m.triIndexes:push_back(#m.vtxs)
				m.vtxs:push_back(mesh.vtxs.v[i+j])
				local v = m.vtxs:back()
				v.pos = v.pos - vec3f(x,y,z)
			end
		end
		assert(#m.triIndexes == #m.vtxs)
		for i=0,#m.triIndexes-1 do
			assert(m.triIndexes.v[i] >= 0 and m.triIndexes.v[i] < #m.vtxs)
		end
		m:rebuildTris()

		-- [[ ok here, per-side, seal off the mesh.

		-- find all open edges, and find what sides they touch ...
		-- TODO what about two triangles, completely overlapping, but varying texcoords?
		-- because that's what's happening on block #4 ...
		--m:mergeMatchingVertexes()

		--[==[ this isn't help anthing
		assert(#m.tris*3 == #m.triIndexes)
		local prec = 1e-5
		local function makekey(i)
			return vec3i(range(0,2):mapi(function(k) return m.triIndexes.v[i+k] end):sort():unpack())
			--[=[
			return range(0,2):mapi(function(k)
				return m.vtxs.v[m.triIndexes.v[i+k]].pos
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
		for i=#m.triIndexes-3,3,-3 do
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

		-- find edges based on vtx comparing pos
		local uniquevs, indexToUniqueV = mesh:getUniqueVtxs(1e-6)
		m:findEdges(function(i)
			return uniquevs[indexToUniqueV[i]]
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


		-- [=[ look at vtxs that are along the bbox
		-- TODO combine this with the edge border loops
		m:calcBBox()
		print('bbox', m.bbox)
		for side=0,2 do
			for pm=0,1 do
				local vs = range(0,#m.vtxs-1):filter(function(i)
					return m.vtxs.v[i].pos.s[side] == m.bbox.s[pm].s[side]
				end)

				-- find what border edges are on this side
				local es = border:mapi(function(e,i,t)
					for j=1,2 do
						if m.vtxs.v[e[j]-1].pos.s[side] ~= m.bbox.s[pm].s[side] then return end
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
				triCount = #m.triIndexes/3,
			},
		}
		--]]
		loader:save(dstfn, m)
	end)
	break
end
