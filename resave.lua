#!/usr/bin/env luajit
local timer = require 'ext.timer'

local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")
local loader = require 'mesh.objloader'()

local mesh = loader:load(infn)
loader:save(outfn, mesh)
