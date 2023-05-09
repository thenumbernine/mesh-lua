#!/usr/bin/env luajit
local Mesh = require 'mesh'
local loader = require 'mesh.objloader'()

local cube = loader:load'cube-rgb.obj'
local r = cube:clone():translate(1,0,0):scale(1, .1, .1)
local g = cube:clone():translate(0,1,0):scale(.1, 1, .1)
local b = cube:clone():translate(0,0,1):scale(.1, .1, 1)

local mesh = r:combine(g, b)
loader:save('axis.obj', mesh)
