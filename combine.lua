#!/usr/bin/env luajit
local Mesh = require 'mesh'
local loader = require 'mesh.objloader'()

local cube = loader:load'cube-rgb.obj'
cube:translate(1,1,1):scale(.5, .5, .5)	-- from [-1,1]^3 to [0,1]^3
local r = cube:clone():scale(.9, .1, .1):translate(.1,0,0)
local g = cube:clone():scale(.1, .9, .1):translate(0,.1,0)
local b = cube:clone():scale(.1, .1, .9):translate(0,0,.1)
local a = cube:clone():scale(.1, .1, .1)

local mesh = r:combine(g, b, a)

-- now break triangles by other triangles ... how so ...
-- triangle/triangle collision ...
mesh:removeInternalTris()

loader:save('axis.obj', mesh)
