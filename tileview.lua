#!/usr/bin/env luajit
local file = require 'ext.file'
local class = require 'ext.class'
local timer = require 'ext.timer'
local table = require 'ext.table'
local tolua = require 'ext.tolua'
local json = require 'dkjson'
local gl = require 'gl'
local GLProgram = require 'gl.program'
local glCall = require 'gl.call'
local ig = require 'imgui'

local matrix_ffi = require 'matrix.ffi'
matrix_ffi.real = 'float'	-- default matrix_ffi type

local OBJLoader = require 'mesh.objloader'

local App = class(require 'imguiapp.withorbit'())


local placefn = assert((...), "expected placement filename")
local d = json.decode(file(placefn):read())
print('# placed', #d.instances)

local instfns = table.mapi(d.instances, function(inst)
	return true, assert(inst.filename, "expected filename")
end):keys():sort()
print('unique files:', tolua(instfns))

local meshesForFns = {}
for _,fn in ipairs(instfns) do
	local base, ext = file(fn):getext()
	local loadfn = fn
	if ext == 'fbx' then
		print(fn.." ... I don't have a Lua FBX loader right now, so ... loading a model instead")
		loadfn = base..'.obj'
	end
	timer('loading '..loadfn, function()
		local mesh = OBJLoader():load(loadfn)
		mesh:mergeMatchingVertexes()
		mesh:generateVertexNormals()
		meshesForFns[fn] = mesh
	end)
end

for _,inst in ipairs(d.instances) do
	inst.mesh = assert(meshesForFns[inst.filename], "failed to find file "..inst.filename)
	inst.transformMat = matrix_ffi{4,4}:lambda(function(i,j)
		return inst.transform[1 + (i-1) + 4 * (j-1)]
	end)
end

function App:initGL(...)
	App.super.initGL(self, ...)

	-- TODO this is in common with view.lua ...
	self.shader = GLProgram{
		vertexCode = [[
#version 460

in vec3 pos;
in vec3 texcoord;
in vec3 normal;
in vec3 com;

uniform vec4 Ka;
uniform vec4 Kd;
uniform vec4 Ks;
uniform float Ns;
uniform bool useFlipTexture;
uniform vec3 objCOM;
uniform vec3 groupCOM;
uniform float groupExplodeDist;
uniform float triExplodeDist;
uniform mat4 modelMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;

out vec3 fragPosv;	// position in view space
out vec3 texcoordv;
out vec3 normalv;
out vec4 Kav;
out vec4 Kdv;
out vec4 Ksv;
out float Nsv;

void main() {
	texcoordv = texcoord;
	if (useFlipTexture) texcoordv.y = 1. - texcoordv.y;
	mat4 modelViewMatrix = viewMatrix * modelMatrix;
	normalv = (modelViewMatrix * vec4(normal, 0.)).xyz;
	Kav = Ka;
	Kdv = Kd;
	Ksv = Ks;
	Nsv = Ns;
	vec3 vertex = pos;
	vertex = mix(vertex, com, triExplodeDist);
	vertex = mix(vertex, groupCOM, groupExplodeDist);
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
in vec3 texcoordv;
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
		diffuseColor *= texture(map_Kd, texcoordv.xy);
	}
	fragColor += diffuseColor;
	if (useLighting) {
		fragColor.xyz *= max(0., dot(normal, lightDir));
	}
	if (useLighting) {
		vec3 viewDir = normalize(-fragPosv);
		vec3 reflectDir = reflect(-lightDir, normal);
		float spec = pow(max(dot(viewDir, reflectDir), 0.), Nsv);
		fragColor += Ksv * spec;
	}
}
]],
		uniforms = {
			useFlipTexture = false,
			useLighting = true,
			lightDir = {1,1,1},
			objCOM = {0,0,0},
			groupCOM = {0,0,0},
			groupExplodeDist = 0,
			triExplodeDist = 0,
			map_Kd = 0,
			Ka = {0,0,0,0},
			Kd = {1,1,1,1},
			Ks = {1,1,1,1},
			Ns = 100,
		},
	}

	for _,mesh in pairs(meshesForFns) do
		mesh:loadGL(self.shader)
	end
	
	gl.glEnable(gl.GL_DEPTH_TEST)
	gl.glEnable(gl.GL_CULL_FACE)
end

App.viewMatrix = matrix_ffi.zeros{4,4}
App.projectionMatrix = matrix_ffi.zeros{4,4}

App.showPoints = false
App.showPointSize = 3
App.showMeshes = true

function App:update()
	gl.glClearColor(0,0,0,0)
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))
	
	gl.glGetFloatv(gl.GL_MODELVIEW_MATRIX, self.viewMatrix.ptr)
	gl.glGetFloatv(gl.GL_PROJECTION_MATRIX, self.projectionMatrix.ptr)

	if self.showMeshes then
		-- TODO this is in common with view.lua ...
		self.shader:use()
		self.shader:setUniforms{
			-- ok this is where things deviate so I can work around the glCall
			viewMatrix = self.viewMatrix.ptr,
			projectionMatrix = self.projectionMatrix.ptr,
		}

		self.list = self.list or {}
		glCall(self.list, function()
			for _,inst in ipairs(d.instances) do
				-- TODO this is in common with view.lua ...
				inst.mesh:draw{
					shader = self.shader,
					beginGroup = function(g)
						if g.tex_Kd then g.tex_Kd:bind() end
						self.shader:setUniforms{
							useTextures = g.tex_Kd and 1 or 0,
							Ka = {0,0,0,0},
							Kd = g.Kd and g.Kd.s or {1,1,1,1},
							Ks = g.Ks and g.Ks.s or {1,1,1,1},
							Ns = g.Ns or 100,
							-- here I'm deviating from view.lua...
							modelMatrix = inst.transformMat.ptr,
						}
					end,	
				}
			end

		end)
		
		self.shader:useNone()
	end
	if self.showPoints then
		gl.glPointSize(self.showPointSize)
		gl.glColor3f(0,1,1)
		gl.glBegin(gl.GL_POINTS)
		for _,inst in ipairs(d.instances) do
			gl.glVertex3fv(inst.transformMat.ptr + 12)
		end
		gl.glEnd()
		gl.glPointSize(1)
	end
	App.super.update(self)
end

function App:updateGUI()
	if ig.igBeginMainMenuBar() then
		if ig.igBeginMenu'Display' then
			ig.luatableCheckbox('show points', self, 'showPoints')
			ig.luatableInputFloat('point size', self, 'showPointSize')
			ig.luatableCheckbox('show meshes', self, 'showMeshes')
			ig.igEndMenu()
		end
		ig.igEndMainMenuBar()
	end
end

App():run()
