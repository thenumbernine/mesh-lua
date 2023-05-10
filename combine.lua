#!/usr/bin/env luajit
local Mesh = require 'mesh'
local timer = require 'ext.timer'

local loader = require 'mesh.objloader'{verbose=true}

local cube
timer('loading', function()
	cube = loader:load'cube-rgb.obj'
end)
cube:translate(1,1,1):scale(.5, .5, .5)	-- from [-1,1]^3 to [0,1]^3
local r = cube:clone():scale(.9, .1, .1):translate(.1,0,0)
local g = cube:clone():scale(.1, .9, .1):translate(0,.1,0)
local b = cube:clone():scale(.1, .1, .9):translate(0,0,.1)
local a = cube:clone():scale(.1, .1, .1)

local mesh = r:combine(g, b, a)

-- now break triangles by other triangles ... how so ...
-- triangle/triangle collision ...
-- another option could be just remove any duplicate tri pairs facing towards one another.
mesh:removeInternalTris()

timer('saving', function()
	loader:save('axis.obj', mesh)
end)
