#!/usr/bin/env luajit
local table = require 'ext.table'
local timer = require 'ext.timer'
local vec3i = require 'vec-ffi.vec3i'

local infn, outfn = ...
assert(infn, "expected "..arg[0].." input-filename")

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

-- 1) slice planes.  somehow avoid planar triangle edges (to prevent slivers)
-- 2) cap off volumes that are sliced.  project down texcoords from above the plane.
-- 3) export blocks one at a time or something

-- or maybe for avoiding slivers ...
-- 1) just group all tris by their integer bounds
-- 2) give them all volumes

local trisForBox = {}
for i=0,mesh.triIndexBuf.size-3,3 do
	local v = mesh.vtxs.v[mesh.triIndexBuf.v[i]].pos
	local iv = vec3i()
	for j=0,2 do
		-- TODO maybe pull slightly towards tri com and against normal
		iv.s[j] = math.floor(v.s[j])
	end
	trisForBox[iv.x] = trisForBox[iv.x] or {}
	trisForBox[iv.x][iv.y] = trisForBox[iv.x][iv.y] or {}
	trisForBox[iv.x][iv.y][iv.z] = trisForBox[iv.x][iv.y][iv.z] or table()
	trisForBox[iv.x][iv.y][iv.z]:insert(i)
end
print('boxes', require 'ext.tolua'(trisForBox))
