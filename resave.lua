#!/usr/bin/env luajit
local timer = require 'ext.timer'

local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")
local loader = require 'mesh.objloader'()

local mesh = loader:load(infn)
mesh:removeUnusedVtxs()

mesh:calcBBox()	-- needed by mesh:mergeMatchingVertexes()
mesh:mergeMatchingVertexes()

mesh:calcTriAux()	-- needed for save()
loader:save(outfn, mesh)
