#!/usr/bin/env luajit
local timer = require 'ext.timer'
local file = require 'ext.file'
local vec3f = require 'vec-ffi.vec3f'
local plane3f = require 'vec-ffi.plane3f'

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

-- in my tri fan implementation this does fill the bottom of map001 correctly
-- in earcut it is failing ...
print'filling in first holes'
mesh:fillHoles()

--[[ test - chop once then fill holes
print'clipping'
mesh:clip(plane3f({1,0,0},-6))
mesh:fillHoles()
mesh:clip(plane3f({-1,0,0},7))
mesh:fillHoles()
mesh:clip(plane3f({0,1,0},-6))
mesh:fillHoles()
mesh:clip(plane3f({0,-1,0},7))
mesh:fillHoles()
mesh:clip(plane3f({0,0,1},-1))
mesh:fillHoles()
mesh:clip(plane3f({0,0,-1},2))
mesh:fillHoles()
--]]

timer('saving', function()
	loader:save(outfn, mesh)
end)

-- [[ now chop up boxes
file'blocks':mkdir()
for i=mesh.bbox.min.x,mesh.bbox.max.x-1 do
	for j=mesh.bbox.min.y,mesh.bbox.max.y-1 do
		for k=mesh.bbox.min.z,mesh.bbox.max.z-1 do
			local block = mesh:clone()
			block:clip(plane3f(vec3f(-1,0,0), i))
			block:fillHoles()
			block:clip(plane3f(vec3f(0,-1,0), j))
			block:fillHoles()
			block:clip(plane3f(vec3f(0,0,-1), k))
			block:fillHoles()
			block:clip(plane3f(vec3f(1,0,0), -i-1))
			block:fillHoles()
			block:clip(plane3f(vec3f(0,1,0), -j-1))
			block:fillHoles()
			block:clip(plane3f(vec3f(0,0,1), -k-1))
			block:fillHoles()
			if block.triIndexes.size > 0 then
				print('generating', i, j, k)
				--print(i,j,k,block.triIndexes.size)
				loader:save('blocks/'..table{i,j,k}:concat'_'..'.obj', block)
			end
		end
	end
end
--]]

