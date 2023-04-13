#!/usr/bin/env luajit
local class = require 'ext.class'
local gl = require 'gl'
local glCallOrRun = require 'gl.call'
local ig = require 'imgui'
local WavefrontObj = require 'wavefrontobj.wavefrontobj'

local fn = assert((...))

local App = class(require 'imguiapp.withorbit'())

App.title = 'WavefrontOBJ preview'

App.enableTextures = true
App.enableLighting = false

function App:initGL(...)
	App.super.initGL(self, ...)

	self.obj = WavefrontObj(fn)
	print('volume', self.obj:calcVolume())

	self:setCenter(self.obj.com3)
	gl.glEnable(gl.GL_DEPTH_TEST)
	gl.glEnable(gl.GL_BLEND)
	gl.glEnable(gl.GL_CULL_FACE)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
	self.displayList = {}
end

function App:update()
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))
	glCallOrRun(self.displayList, function()
		if self.enableLighting then
			gl.glEnable(gl.GL_LIGHTING)
			gl.glEnable(gl.GL_LIGHT0)
		end
		self.obj:draw{
			disableTextures = not self.enableTextures,
			-- TODO option for calculated normals
		}
		if self.enableLighting then
			gl.glDisable(gl.GL_LIGHTING)
			gl.glDisable(gl.GL_LIGHT0)
		end
	end)
	App.super.update(self)
	require 'gl.report''here'
end

function App:deleteDisplayList()
	if self.displayList.id then
		gl.glDeleteLists(self.displayList.id, 1)
		self.displayList.id = nil
	end
end

function App:setCenter(center)
	local size = self.obj.vs:mapi(function(v) return (v - center):length() end):sup()
	self.view.orbit:set(center:unpack())
	self.view.pos = self.view.orbit + self.view.angle:zAxis() * size
end

function App:updateGUI()
	if ig.igButton'set to vtx center' then
		self:setCenter(self.obj.com0)
	end
	if ig.igButton'set to line center' then
		self:setCenter(self.obj.com1)
	end
	if ig.igButton'set to face center' then
		self:setCenter(self.obj.com2)
	end
	if ig.igButton'set to volume center' then
		self:setCenter(self.obj.com3)
	end
	-- TODO D3 for volume-centered
	ig.luatableCheckbox('ortho', self.view, 'ortho')
	if ig.igButton'reset view' then
		self.view.ortho = false
		self.view.angle:set(0,0,0,1)
		self:setCenterD0()
	end
	if ig.luatableCheckbox('use textures', self, 'enableTextures') then
		self:deleteDisplayList()
	end
	if ig.luatableCheckbox('use lighting', self, 'enableLighting') then
		self:deleteDisplayList()
	end
end

App():run()
