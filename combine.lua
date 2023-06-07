#!/usr/bin/env luajit
local Mesh = require 'mesh'
local timer = require 'ext.timer'
local quatf = require 'vec-ffi.quatf'
local loader = require 'mesh.objloader'{verbose=true}

local cube
timer('loading', function()
	cube = loader:load'cube-rgb.obj'
end)

--[[
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
--]]

-- [[
-- bbox ((-0.15080007910728, -0.10661125928164, -1.2188935279846), (0.15080018341541, 0.013977702707052, -0.71889358758926
-- bbox size (0.3, 0.12, 0.5)
local mesh = Mesh():combine(
	cube:clone():scale(.02, .02, .25):translate(.13, 0, -.95),
	cube:clone():scale(.02, .02, .25):translate(-.13, 0, -.95),
	cube:clone():scale(.09, .02, .25):rotate(quatf():fromAngleAxis(0,0,1,-30)):translate(.062, 0.04, -.95),
	cube:clone():scale(.09, .02, .25):rotate(quatf():fromAngleAxis(0,0,1,30)):translate(-.062, 0.04, -.95)
)
timer('saving', function()
	loader:save('roof_hip_approx.obj', mesh)
end)
--]]

