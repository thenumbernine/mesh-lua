#!/usr/bin/env luajit
local timer = require 'ext.timer'

local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")
local loader = require 'mesh.objloader'{verbose=true}

local mesh
timer('loading', function()
	mesh = loader:load(infn)
end)
--mesh:scale(.33*.5, .025*.5, .42*.5)
timer('saving', function()
	loader:save(outfn, mesh)
end)
