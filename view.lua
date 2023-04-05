#!/usr/bin/env luajit
local class = require 'ext.class'
local gl = require 'gl'
local glCallOrRun = require 'gl.call'
local ig = require 'imgui'
local WavefrontObj = require 'wavefrontobj.wavefrontobj'

local fn = assert((...))

local App = class(require 'imguiapp.withorbit'())

App.title = 'WavefrontOBJ preview'

function App:initGL(...)
	App.super.initGL(self, ...)
	self.obj = WavefrontObj(fn)
	self:setCenterD0()
	gl.glEnable(gl.GL_DEPTH_TEST)
	self.displayList = {}
end

function App:update()
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))
	glCallOrRun(self.displayList, function()
		self.obj:draw()
	end)
	App.super.update(self)
end

-- center by vtx avg
function App:setCenterD0()
	self:setCenter(self.obj.vs:sum() / #self.obj.vs)
end

-- center by edge avg
-- TODO track all edges
-- use matching vtx pos or vtx index?
function App:setCenterD1()
	self:setCenter(self.obj.vs:sum() / #self.obj.vs)
end

function App:setCenterD2()
end

function App:setCenter(center)
	local size = self.obj.vs:mapi(function(v) return (v - center):length() end):sup()
	self.view.orbit:set(center:unpack())
	self.view.pos = self.view.orbit + self.view.angle:zAxis() * size
end

function App:updateGUI()
	if ig.igButton'update to vtx center' then
		self:setCenterD0()
	end
	ig.luatableCheckbox('ortho', self.view, 'ortho')
	if ig.igButton'reset view' then
		self.view.ortho = false
		self.view.angle:set(0,0,0,1)
		self:setCenterD0()
	end
end

App():run()
