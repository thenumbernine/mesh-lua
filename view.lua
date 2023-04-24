#!/usr/bin/env luajit
local class = require 'ext.class'
local gl = require 'gl'
local glCallOrRun = require 'gl.call'
local ig = require 'imgui'
local WavefrontObj = require 'wavefrontobj.wavefrontobj'
local vec4f = require 'vec-ffi.vec4f'

local fn = assert((...))

local App = class(require 'imguiapp.withorbit'())

App.title = 'WavefrontOBJ preview'

function App:initGL(...)
	App.super.initGL(self, ...)

	self.obj = WavefrontObj(fn)
	print('volume', self.obj:calcVolume())

	self:setCenter(self.obj.com3)
	self.displayList = {}

	-- gui options
	self.enableWireframe = false
	self.enableTextures = true
	self.enableLighting = false
	self.enableNearest = false
	self.bgcolor = vec4f(.2, .3, .5, 1)
end

function App:update()
	gl.glEnable(gl.GL_DEPTH_TEST)
	gl.glEnable(gl.GL_BLEND)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
	gl.glEnable(gl.GL_CULL_FACE)
	gl.glClearColor(self.bgcolor:unpack())
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))
	if self.enableWireframe then
		gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_LINE)
	end
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
	if self.enableWireframe then
		gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_FILL)
	end
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
	ig.igColorPicker3('background color', self.bgcolor.s, 0)
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
	ig.luatableCheckbox('ortho view', self.view, 'ortho')
	if ig.igButton'reset view' then
		self.view.ortho = false
		self.view.angle:set(0,0,0,1)
		self:setCenterD0()
	end
	ig.luatableCheckbox('wireframe', self, 'enableWireframe')
	if ig.luatableCheckbox('use textures', self, 'enableTextures') then
		self:deleteDisplayList()
	end
	if ig.luatableCheckbox('nearest filter', self, 'enableNearest') then
		for mtlname, mtl in pairs(self.obj.mtllib) do
			if mtl.tex_Kd then
				mtl.tex_Kd:bind()
				mtl.tex_Kd:setParameter(gl.GL_TEXTURE_MAG_FILTER, self.enableNearest and gl.GL_NEAREST or gl.GL_LINEAR)
				mtl.tex_Kd:unbind()
			end
		end
	end
	if ig.luatableCheckbox('use lighting', self, 'enableLighting') then
		self:deleteDisplayList()
	end
end

App():run()
