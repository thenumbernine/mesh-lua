#!/usr/bin/env luajit
local class = require 'ext.class'
local gl = require 'gl'
local glCallOrRun = require 'gl.call'
local WavefrontObj = require 'wavefrontobjviewer.wavefrontobj'

local fn = assert((...))

local App = class(require 'imguiapp.withorbit'())

App.title = 'WavefrontOBJ preview'

function App:initGL(...)
	App.super.initGL(self, ...)
	self.obj = WavefrontObj(fn)
	local center = self.obj.vs:sum() / #self.obj.vs
	local size = self.obj.vs:mapi(function(v) return (v - center):length() end):sup()
	self.view.orbit:set(center:unpack())
	self.view.pos = self.view.orbit + self.view.angle:zAxis() * size
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

App():run()
