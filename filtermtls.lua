#!/usr/bin/env luajit
local timer = require 'ext.timer'
local table = require 'ext.table'

local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")
local loader = require 'mesh.objloader'()

-- use quotes around the args ...
local keepers = table{...}:sub(3):mapi(function(x)
	return true, x
end):setmetatable(nil)
print('keeping '..require'ext.tolua'(keepers))

local mesh = loader:load(infn)

for mtlname in pairs(keepers) do
	print(mtlname, mesh:getTriIndexesForMaterial(mtlname))
end

local ms = table.map(mesh.mtllib, function(m,i,t)
	return m, #t+1
end)

-- verify material tri index have no overlaps
for i=1,#ms-1 do
	local i1,i2 = mesh:getTriIndexesForMaterial(ms[i].name)
	if i2 >= i1 then
		for j=i+1,#ms do
			local j1,j2 = mesh:getTriIndexesForMaterial(ms[j].name)
			if j2 > j1 then
				if not (i2 < j1 or i1 > j2) then
					error("overlapping meshes "..ms[i].name.." and "..ms[j].name
						.." with ranges "
						..require'ext.tolua'{[ms[i].name]={i1,i2},[ms[j].name]={j1,j2}})
				end
			end
		end
	end
end

-- verify material tri count == mesh tri count
local totalMtlTris = ms:mapi(function(m) return m.triCount end):sum()
if totalMtlTris ~= #mesh.tris then
	error("expected "..totalMtlTris .." == " .. #mesh.tris)
end

print('before filtering faces, #tris', #mesh.tris)
-- TODO change removeTri() to removeTriRange()
timer('filtering faces', function()
	for mtlname,mtl in pairs(mesh.mtllib) do
		if keepers[mtlname] then
			print('keeping material '..mtlname)
		else
			print('remove material '..mtlname)
			local triCount = mtl.triCount
			print('material has num tris '..triCount)
			local numTrisBefore = #mesh.tris
			print('before #tris', numTrisBefore)
			
			-- [[ removeMaterial:	
			local i1, i2 = mesh:getTriIndexesForMaterial(mtlname)
			for i=i2,i1,-1 do
				mesh.tris:remove(i)
			end
			for mtlname2,mtl2 in pairs(mesh.mtllib) do
				if mtlname ~= mtlname2 then
					if i2 < mtl2.triFirstIndex then
						mtl2.triFirstIndex = mtl2.triFirstIndex - mtl.triCount
					end
				end
			end
			mtl.triCount = 0
			--]]
			
			local numTrisAfter = #mesh.tris
			print('after #tris', numTrisAfter)
			if numTrisBefore - numTrisAfter ~= triCount then
				error("expected to lose "..triCount.." but lost "..(numTrisBefore - numTrisAfter))
			end
		end
	end
end)
print('after filtering faces, #tris', #mesh.tris)

-- filter unused material
for _,mtlname in ipairs(table.keys(mesh.mtllib)) do
	local m = mesh.mtllib[mtlname]
	if m.triCount == 0 and m.name ~= '' then
		mesh.mtllib[mtlname] = nil
	end
end

print('before removing unused vtxs, #tris', #mesh.tris)
mesh:removeUnusedVtxs()
print('after removing unused vtxs, #tris', #mesh.tris)

-- now remove any vertexes not used ...
-- ... and make it a function

mesh:calcBBox()	-- needed for threshold for mergeMatchingVertexes
print('before merging vtxs, #tris', #mesh.tris)
mesh:mergeMatchingVertexes()
print('after merging vtxs, #tris', #mesh.tris)

mesh:calcTriAux()	-- needed for save()
loader:save(outfn, mesh)
