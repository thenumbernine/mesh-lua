#!/usr/bin/env luajit
local class = require 'ext.class'
local gl = require 'gl'
local GLProgram = require 'gl.program'
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
	self.useWireframe = false
	self.useTextures = true
	self.useLighting = false
	self.useTexFilterNearest = false
	self.explodeDist = 0
	self.bgcolor = vec4f(.2, .3, .5, 1)

	self.shader = GLProgram{
		vertexCode = [[
varying vec2 texCoordv;
varying vec4 colorv;

uniform vec3 offset;

void main() {
	texCoordv = gl_MultiTexCoord0.xy;
	colorv = gl_Color;
	vec3 vertex = gl_Vertex.xyz;
	vertex += offset;
	gl_Position = gl_ProjectionMatrix * (gl_ModelViewMatrix * vec4(vertex, 1.));
}
]],
		fragmentCode = [[
uniform sampler2D tex;
uniform bool useLighting;
uniform bool useTextures;

varying vec2 texCoordv;
varying vec4 colorv;

void main() {
	gl_FragColor = colorv;
	if (useLighting) {
	}
	if (useTextures) {
		gl_FragColor *=texture2D(tex, texCoordv);
	}
}
]],
		uniforms = {
			tex = 0,
		},
	}
end

function App:update()
	gl.glEnable(gl.GL_DEPTH_TEST)
	gl.glEnable(gl.GL_BLEND)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
	gl.glEnable(gl.GL_CULL_FACE)
	gl.glClearColor(self.bgcolor:unpack())
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))
	if self.useWireframe then
		gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_LINE)
	end
--	glCallOrRun(self.displayList, function()
		self.shader:use()
		if self.shader.uniforms.useLighting then
			gl.glUniform1i(self.shader.uniforms.useLighting.loc, self.useLighting and 1 or 0)
		end
		self.obj:draw{
			-- TODO option for calculated normals?
			-- TODO shader options?
			beginMtl = function(mtl)
				local useTextures = mtl.tex_Kd and self.useTextures
				gl.glUniform1i(self.shader.uniforms.useTextures.loc, useTextures and 1 or 0)

				local offset = (mtl.com3 - self.obj.com3) * self.explodeDist
				gl.glUniform3f(self.shader.uniforms.offset.loc, offset:unpack())
			end,
		}
		self.shader:useNone()
--	end)
	if self.useWireframe then
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
	ig.luatableCheckbox('ortho view', self.view, 'ortho')
	if ig.igButton'reset view' then
		self.view.ortho = false
		self.view.angle:set(0,0,0,1)
		self:setCenterD0()
	end
	-- TODO max dependent on bounding radius of model, same with COM camera positioning
	ig.luatableSliderFloat('explode dist', self, 'explodeDist', 0, 2)
	ig.luatableCheckbox('wireframe', self, 'useWireframe')
	if ig.luatableCheckbox('use textures', self, 'useTextures') then
		self:deleteDisplayList()
	end
	if ig.luatableCheckbox('nearest filter', self, 'useTexFilterNearest') then
		for mtlname, mtl in pairs(self.obj.mtllib) do
			if mtl.tex_Kd then
				mtl.tex_Kd:bind()
				mtl.tex_Kd:setParameter(gl.GL_TEXTURE_MAG_FILTER, self.useTexFilterNearest and gl.GL_NEAREST or gl.GL_LINEAR)
				mtl.tex_Kd:unbind()
			end
		end
	end
	if ig.luatableCheckbox('use lighting', self, 'useLighting') then
		self:deleteDisplayList()
	end

	-- TODO per-mesh explode coeff for viewing distinct pieces
	-- then maybe per-tri as well?
end

App():run()
