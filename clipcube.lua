#!/usr/bin/env luajit
local plane3f = require 'vec-ffi.plane3f'
local timer = require 'ext.timer'

local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")
local loader = require 'mesh.objloader'()

local mesh
timer('loading', function()
	mesh = loader:load(infn)
end)
--mesh:breakAllVertexes()

-- first clip
mesh:clip(plane3f({0,1,0},0))

-- then fill in holes
mesh:fillHoles()

timer('saving', function()
	loader:save(outfn, mesh)
end)
