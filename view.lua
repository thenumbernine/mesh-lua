#!/usr/bin/env luajit
local class = require 'ext.class'
local gl = require 'gl'
local GLProgram = require 'gl.program'
local glCallOrRun = require 'gl.call'
local ig = require 'imgui'
local OBJLoader = require 'wavefrontobj.objloader'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'
local matrix_ffi = require 'matrix.ffi'
matrix_ffi.real = 'float'	-- default matrix_ffi type

local fn = assert((...))

local App = class(require 'imguiapp.withorbit'())

App.title = 'WavefrontOBJ preview'

function App:initGL(...)
	App.super.initGL(self, ...)

	self.obj = OBJLoader():load(fn)
	print('triangle bounded volume', self.obj:calcVolume())
	print('bbox', self.obj.bbox)
	print('bbox volume', (self.obj.bbox.max - self.obj.bbox.min):volume())
	print('obj.bbox corner-to-corner distance: '..(self.obj.bbox.max - self.obj.bbox.min):norm())

	-- [[ default camera to ortho looking down y-
	self.view.ortho = true
	self.view.angle:fromAngleAxis(1,0,0,-90)
	--]]

	self:setCenter(self.obj.com3)
	self.displayList = {}

	-- gui options
	self.useWireframe = false
	self.useDrawEdges = false
	self.useDrawPolys = true
	self.drawStoredNormals = false
	self.drawVertexNormals = false
	self.drawTriNormals = false
	self.drawUVs = false
	self.drawUVs3D = true
	self.drawUVUnwrapEdges = false
	self.useTextures = true
	self.useFlipTexture = false	-- opengl vs directx? v=0 is bottom or top?
	self.useTexFilterNearest = false
	
	self.useLighting = false
	self.useGeneratedNormalsForLighting = false
	self.lightDir = vec3f(1,1,1)

	self.useCullFace = true
	self.useDepthTest = true
	self.useBlend = true
	self.groupExplodeDist = 0
	self.triExplodeDist = 0
	self.bgcolor = vec4f(.2, .3, .5, 1)

	self.shader = GLProgram{
		vertexCode = [[
#version 460

in vec3 pos;
in vec3 texCoord;
in vec3 normal;		//mesh-provided normal
in vec3 normal2;	//generated normal
in vec3 com;

uniform bool useNormal2;
uniform bool useFlipTexture;
uniform vec4 Ka;
uniform vec4 Kd;
uniform vec4 Ks;
uniform float Ns;
uniform vec3 objCOM;
uniform vec3 groupCOM;
uniform float groupExplodeDist;
uniform float triExplodeDist;
uniform mat4 modelViewMatrix;
uniform mat4 projectionMatrix;

out vec3 fragPosv;	// position in view space
out vec3 texCoordv;
out vec3 normalv;
out vec4 Kav;
out vec4 Kdv;
out vec4 Ksv;
out float Nsv;

void main() {
	texCoordv = texCoord;
	if (useFlipTexture) texCoordv.y = 1. - texCoordv.y;
	normalv = (modelViewMatrix * vec4(useNormal2 ? normal : normal2, 0.)).xyz;
	Kav = Ka;
	Kdv = Kd;
	Ksv = Ks;
	Nsv = Ns;
	vec3 groupExplodeOffset = (groupCOM - objCOM) * groupExplodeDist;
	vec3 triExplodeOffset = (com - groupCOM) * triExplodeDist;
	vec3 vertex = pos + groupExplodeOffset + triExplodeOffset;
	vec4 fragPos = modelViewMatrix * vec4(vertex, 1.);
	fragPosv = fragPos.xyz;
	gl_Position = projectionMatrix * fragPos;
}
]],
		fragmentCode = [[
#version 460

uniform sampler2D map_Kd;
uniform bool useLighting;
uniform vec3 lightDir;
uniform bool useTextures;

in vec3 fragPosv;
in vec3 texCoordv;
in vec3 normalv;
in vec4 Kav;
in vec4 Kdv;
in vec4 Ksv;
in float Nsv;

out vec4 fragColor;

void main() {
	vec3 normal = normalize(normalv);
	fragColor = Kav;
	vec4 diffuseColor = Kdv;
	if (useTextures) {
		diffuseColor *= texture(map_Kd, texCoordv.xy);
	}
	fragColor += diffuseColor;
	if (useLighting) {
		fragColor.xyz *= max(0., dot(normal, lightDir));
	}
	if (useLighting) {
		vec3 viewPos = vec3(0., 0., 0.);
		vec3 viewDir = normalize(viewPos - fragPosv);
		vec3 reflectDir = reflect(-lightDir, normal);
		float spec = pow(max(dot(viewDir, reflectDir), 0.), Nsv);
		fragColor += Ksv * spec;
	}
}
]],
		uniforms = {
			objCOM = {0,0,0},
			groupCOM = {0,0,0},
			groupExplodeDist = 0,
			triExplodeDist = 0,
			map_Kd = 0,
			Ka = {0,0,0,0},
			Kd = {1,1,1,1},
			Ks = {1,1,1,1},
			Ns = 1,
		},
	}

	self.obj:loadGL(self.shader)
end

App.modelViewMatrix = matrix_ffi.zeros{4,4}
App.projectionMatrix = matrix_ffi.zeros{4,4}

function App:update()
	gl.glClearColor(self.bgcolor:unpack())
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))

	gl.glDepthFunc(gl.GL_LEQUAL)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
	
	if self.useDepthTest then
		gl.glEnable(gl.GL_DEPTH_TEST)
	end
	if self.useBlend then
		gl.glEnable(gl.GL_BLEND)
	end
	if self.useCullFace then
		--gl.glFrontFace(gl.GL_CCW)
		--gl.glCullFace(gl.GL_BACK)
		gl.glEnable(gl.GL_CULL_FACE)
	end
	if self.useWireframe then
		gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_LINE)
	end

	gl.glGetFloatv(gl.GL_MODELVIEW_MATRIX, self.modelViewMatrix.ptr)
	gl.glGetFloatv(gl.GL_PROJECTION_MATRIX, self.projectionMatrix.ptr)

	self.obj:loadGL(self.shader)

	if self.drawStoredNormals then
		self.obj:drawStoredNormals()
	end
	if self.drawVertexNormals then
		self.obj:drawVertexNormals()
	end
	if self.drawTriNormals then
		self.obj:drawTriNormals()
	end
	if self.useDrawPolys then
		self.shader:use()
		self.shader:setUniforms{
			useFlipTexture = self.useFlipTexture,
			useNormal2 = self.useGeneratedNormalsForLighting,
			useLighting = self.useLighting,
			lightDir = self.lightDir:normalize().s,
			modelViewMatrix = self.modelViewMatrix.ptr,
			projectionMatrix = self.projectionMatrix.ptr,
		}
		self.obj:draw{
			-- TODO option for calculated normals?
			-- TODO shader options?
			shader = self.shader,
			beginMtl = function(mtl)
				if mtl.tex_Kd then mtl.tex_Kd:bind() end
				self.shader:setUniforms{
					useTextures = self.useTextures and mtl.tex_Kd and 1 or 0,
					--Ka = mtl.Ka or {0,0,0,0},	-- why are most obj files 1,1,1,1 ambient?  because blender exports ambient as 1,1,1,1 ... but that would wash out all lighting ... smh
					Ka = {0,0,0,0},
					Kd = mtl.Kd or {1,1,1,1},
					Ks = mtl.Ks or {1,1,1,1},
					Ns = mtl.Ns or 1,
					objCOM = self.obj.com3,
					groupCOM = mtl.com3,
					groupExplodeDist = self.groupExplodeDist,
					triExplodeDist = self.triExplodeDist,
				}
			end,
		}
		self.shader:useNone()
	end
	if self.drawUVs then
		self.obj:drawUVs(self.drawUVs3D)
	end
	if self.drawUVUnwrapEdges then
		self.obj:drawUVUnwrapEdges(self.drawUVs3D)
	end
	if self.useDrawEdges then
		self.obj:drawEdges(self.triExplodeDist, self.groupExplodeDist)
	end

	gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_FILL)
	gl.glDisable(gl.GL_BLEND)
	gl.glDisable(gl.GL_CULL_FACE)
	App.super.update(self)
	require 'gl.report''here'
end

function App:setCenter(center)
	local size = self.obj.vs:mapi(function(v) return (v - center):norm() end):sup()
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
	if ig.igButton'reset view z-' then
		self.view.angle:set(0,0,0,1)
		self:setCenter(self.obj.com3)
	end
	if ig.igButton'reset view z+' then
		self.view.angle:fromAngleAxis(0,1,0,180)
		self:setCenter(self.obj.com3)
	end
	if ig.igButton'reset view y-' then
		self.view.angle:fromAngleAxis(1,0,0,-90)
		self:setCenter(self.obj.com3)
	end
	if ig.igButton'reset view y+' then
		self.view.angle:fromAngleAxis(1,0,0,90)
		self:setCenter(self.obj.com3)
	end
	if ig.igButton'reset view x-' then
		self.view.angle:fromAngleAxis(0,1,0,90)
		self:setCenter(self.obj.com3)
	end
	if ig.igButton'reset view x+' then
		self.view.angle:fromAngleAxis(0,1,0,-90)
		self:setCenter(self.obj.com3)
	end


	ig.luatableCheckbox('use cull face', self, 'useCullFace')
	ig.luatableCheckbox('use depth test', self, 'useDepthTest')
	ig.luatableCheckbox('use blend', self, 'useBlend')
	ig.luatableCheckbox('use textures', self, 'useTextures')
	ig.luatableCheckbox('flip texture', self, 'useFlipTexture')
	if ig.luatableCheckbox('nearest filter', self, 'useTexFilterNearest') then
		for mtlname, mtl in pairs(self.obj.mtllib) do
			if mtl.tex_Kd then
				mtl.tex_Kd:bind()
				mtl.tex_Kd:setParameter(gl.GL_TEXTURE_MAG_FILTER, self.useTexFilterNearest and gl.GL_NEAREST or gl.GL_LINEAR)
				mtl.tex_Kd:unbind()
			end
		end
	end
	ig.luatableCheckbox('use lighting', self, 'useLighting')
	ig.luatableCheckbox('use generated normals for lighting', self, 'useGeneratedNormalsForLighting')
	
	-- TODO max dependent on bounding radius of model, same with COM camera positioning
	-- TODO per-tri exploding as well
	ig.luatableSliderFloat('mtl explode dist', self, 'groupExplodeDist', 0, 2)
	ig.luatableSliderFloat('tri explode dist', self, 'triExplodeDist', 0, 2)
	ig.luatableCheckbox('wireframe', self, 'useWireframe')
	ig.luatableCheckbox('draw edges', self, 'useDrawEdges')
	ig.luatableCheckbox('draw polys', self, 'useDrawPolys')
	ig.luatableCheckbox('draw stored normals', self, 'drawStoredNormals')
	ig.luatableCheckbox('draw vertex normals', self, 'drawVertexNormals')
	ig.luatableCheckbox('draw tri normals', self, 'drawTriNormals')
	ig.luatableCheckbox('draw uvs', self, 'drawUVs')
	ig.luatableCheckbox('draw uvs 3D', self, 'drawUVs3D')
	ig.luatableCheckbox('draw uv unwrap edges', self, 'drawUVUnwrapEdges')
end

App():run()
