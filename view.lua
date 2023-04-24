#!/usr/bin/env luajit
local class = require 'ext.class'
local gl = require 'gl'
local GLProgram = require 'gl.program'
local glCallOrRun = require 'gl.call'
local ig = require 'imgui'
local WavefrontObj = require 'wavefrontobj.wavefrontobj'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'
local matrix_ffi = require 'matrix.ffi'
matrix_ffi.real = 'float'	-- default matrix_ffi type

local fn = assert((...))

local App = class(require 'imguiapp.withorbit'())

App.title = 'WavefrontOBJ preview'

function App:initGL(...)
	App.super.initGL(self, ...)

	self.obj = WavefrontObj(fn)
	print('volume', self.obj:calcVolume())
	print('bbox volume', (self.obj.bbox.max - self.obj.bbox.min):volume())

	self:setCenter(self.obj.com3)
	self.displayList = {}

	-- gui options
	self.useWireframe = false
	self.useTextures = true
	self.useLighting = false
	self.useCullFace = true
	self.useDepthTest = true
	self.useBlend = true
	self.useTexFilterNearest = false
	self.explodeDist = 0
	self.bgcolor = vec4f(.2, .3, .5, 1)

	self.shader = GLProgram{
		vertexCode = [[
#version 460

in vec3 pos;
in vec2 texCoord;
in vec3 normal;
in vec3 com;

uniform vec4 color;
uniform vec3 offset;	//per-material
uniform mat4 modelViewMatrix;
uniform mat4 projectionMatrix;

out vec2 texCoordv;
out vec3 normalv;
out vec4 colorv;

void main() {
	texCoordv = texCoord;
	normalv = normal;
	colorv = color;
	vec3 vertex = pos + offset;
	gl_Position = projectionMatrix * (modelViewMatrix * vec4(vertex, 1.));
}
]],
		fragmentCode = [[
#version 460

uniform sampler2D tex;
uniform bool useLighting;
uniform bool useTextures;

in vec2 texCoordv;
in vec3 normalv;
in vec4 colorv;

out vec4 fragColor;

void main() {
	fragColor = colorv;
	if (useLighting) {
		fragColor.rgb *= dot(normalv, vec3(1., 1., 1.));
	}
	if (useTextures) {
		fragColor *= texture(tex, texCoordv);
	}
}
]],
		uniforms = {
			tex = 0,
			color = {1,1,1,1},
		},
	}

	self.obj:loadGL(self.shader)
end

App.modelViewMatrix = matrix_ffi.zeros{4,4}
App.projectionMatrix = matrix_ffi.zeros{4,4}

function App:update()
	if self.useDepthTest then
		gl.glEnable(gl.GL_DEPTH_TEST)
	end
	if self.useBlend then
		gl.glEnable(gl.GL_BLEND)
		gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
	end
	if self.useCullFace then
		--gl.glFrontFace(gl.GL_CCW)
		--gl.glCullFace(gl.GL_BACK)
		gl.glEnable(gl.GL_CULL_FACE)
	end
	gl.glClearColor(self.bgcolor:unpack())
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))
	if self.useWireframe then
		gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_LINE)
	end

	gl.glGetFloatv(gl.GL_MODELVIEW_MATRIX, self.modelViewMatrix.ptr)
	gl.glGetFloatv(gl.GL_PROJECTION_MATRIX, self.projectionMatrix.ptr)

--	glCallOrRun(self.displayList, function()
		self.shader:use()
		if self.shader.uniforms.useLighting then
			gl.glUniform1i(self.shader.uniforms.useLighting.loc, self.useLighting and 1 or 0)
		end
		self.obj:draw{
			-- TODO option for calculated normals?
			-- TODO shader options?
			shader = self.shader,
			beginMtl = function(mtl)
				if mtl.tex_Kd then mtl.tex_Kd:bind() end
				self.shader:setUniforms{
					useTextures = mtl.tex_Kd and self.useTextures and 1 or 0,
					color = mtl.Kd or {1,1,1,1},
					offset = vec3f(((mtl.com3 - self.obj.com3) * self.explodeDist):unpack()).s,
					modelViewMatrix = self.modelViewMatrix.ptr,
					projectionMatrix = self.projectionMatrix.ptr,
				}
			end,
		}
		self.shader:useNone()
--	end)
	gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_FILL)
	gl.glDisable(gl.GL_BLEND)
	gl.glDisable(gl.GL_CULL_FACE)
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
		self:setCenter(self.obj.com3)
	end
	-- TODO max dependent on bounding radius of model, same with COM camera positioning
	ig.luatableSliderFloat('explode dist', self, 'explodeDist', 0, 2)
	ig.luatableCheckbox('wireframe', self, 'useWireframe')
	ig.luatableCheckbox('use cull face', self, 'useCullFace')
	ig.luatableCheckbox('use depth test', self, 'useDepthTest')
	ig.luatableCheckbox('use blend', self, 'useBlend')
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
