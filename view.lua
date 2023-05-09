#!/usr/bin/env luajit
local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local timer = require 'ext.timer'
local gl = require 'gl'
local GLProgram = require 'gl.program'
local glCallOrRun = require 'gl.call'
local ig = require 'imgui'
local vec3f = require 'vec-ffi.vec3f'
local vec3d = require 'vec-ffi.vec3d'
local vec4f = require 'vec-ffi.vec4f'
local quatd = require 'vec-ffi.quatd'
local matrix_ffi = require 'matrix.ffi'
local cmdline = require 'ext.cmdline'(...)
local OBJLoader = require 'mesh.objloader'
local unwrapUVs = require 'mesh.unwrapuvs'.unwrapUVs
local drawUVUnwrapEdges = require 'mesh.unwrapuvs'.drawUVUnwrapEdges
local tileMesh = require 'mesh.tilemesh'.tileMesh
local drawTileMeshPlaces = require 'mesh.tilemesh'.drawTileMeshPlaces
matrix_ffi.real = 'float'	-- default matrix_ffi type

local fn = cmdline.file
if not fn then fn = ... end
if not fn then error("can't figure out what your file is from the cmdline") end

local App = class(require 'imguiapp.withorbit'())

App.title = 'WavefrontOBJ preview'

local dirnames = table{
	'x+',
	'x-',
	'y+',
	'y-',
	'z+',
	'z-',
}
local dirs = table{
	{1,0,0},
	{-1,0,0},
	{0,1,0},
	{0,-1,0},
	{0,0,1},
	{0,0,-1},
}

function App:initGL(...)
	App.super.initGL(self, ...)

	self.unwrapAngleThreshold = 5

	self.view.znear = .1
	self.view.zfar = 40000

	self.mesh = OBJLoader():load(fn)
print('#unique vertexes', self.mesh.vtxs.size)
print('#unique triangles', self.mesh.triIndexes.size/3)

	-- TODO how to request this?  dirty bits?
	self.mesh:prepare()

	if cmdline.tribasis then
		self.mesh:generateTriBasis()
	end

	-- TODO make this an option with specified threshold.
	-- calcBBox has to be done first
	-- after doing this you have to call findEdges and calcCOMs
	if cmdline.mergevtxs then
		timer('merging vertexes', function()
			self.mesh:mergeMatchingVertexes()
		end)
		-- refresh edges, com0, and com1
		self.mesh:findEdges()
		self.mesh.com0 = self.mesh:calcCOM0()
		self.mesh.com1 = self.mesh:calcCOM1()
	end

	-- TODO give every vtx a TNB, use it instead of uvbasis3D, and don't have tilemesh require unwrapuv
	if cmdline.unwrapuv then
-- [[ calculate unique volumes / calculate any distinct pieces on them not part of the volume
		timer('unwrapping uvs', function()
			unwrapUVs{
				mesh = self.mesh,
				angleThreshold = self.unwrapAngleThreshold,
			}
		end)
	end
	if cmdline.tilemesh then
		tileMesh(self.mesh, OBJLoader():load(cmdline.tilemesh))
	end
--]]

	print('triangle bounded volume', self.mesh:calcVolume())
	print('bbox', self.mesh.bbox)
	print('bbox volume', (self.mesh.bbox.max - self.mesh.bbox.min):volume())
	print('mesh.bbox corner-to-corner distance: '..(self.mesh.bbox.max - self.mesh.bbox.min):norm())

--[[ default camera to ortho looking down y-
	self.view.ortho = true
	self.view.angle:fromAngleAxis(1,0,0,-90)
	self.updirIndex = dirnames:find'z+'
--]]
-- [[ default opengl mode
	self.updirIndex = dirnames:find'y+'
	self:resetAngle(vec3d(0,0,-1))	-- z-back
--]]

	if cmdline.up then
		self.updirIndex = dirnames:find(cmdline.up)
		self:resetAngle()
	end
	if cmdline.fwd then
		self:resetAngle(vec3d(table.unpack(dirs[dirnames:find(cmdline.fwd)])))
	end

	-- gui options
	self.useWireframe = false
	self.useDrawVertexes = false
	self.useDrawBBox = false
	self.useDrawEdges = false
	self.useDrawPolys = true
	self.drawVertexNormals = false
	self.drawTriNormals = false
	self.drawTriBasis = false
	self.useTextures = true
	self.useFlipTexture = false	-- opengl vs directx? v=0 is bottom or top?
	self.useTexFilterNearest = false

	self.drawUVUnwrapEdges = false
	self.drawTileMeshPlaces = false

	self.editMode = 1

	self.useLighting = false
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
in vec3 texcoord;
in vec3 normal;
in vec3 com;

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
out vec3 texcoordv;
out vec3 normalv;
out vec4 Kav;
out vec4 Kdv;
out vec4 Ksv;
out float Nsv;

void main() {
	texcoordv = texcoord;
	if (useFlipTexture) texcoordv.y = 1. - texcoordv.y;
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

	self.mesh:loadGL(self.shader)
end

App.modelViewMatrix = matrix_ffi.zeros{4,4}
App.projectionMatrix = matrix_ffi.zeros{4,4}

function App:update()
	local mesh = self.mesh

	gl.glClearColor(self.bgcolor:unpack())
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))

	gl.glDepthFunc(gl.GL_LEQUAL)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)

	gl.glDepthMask(gl.GL_FALSE)
	local axisw = math.ceil(math.min(self.width, self.height) / 4)
	gl.glViewport(self.width-1-axisw, self.height-1-axisw, axisw, axisw)
	local pushOrtho = self.view.ortho
	self.view.ortho = false
	self.view:setup(1)
	local aa = self.view.angle:conjugate():toAngleAxis()
	gl.glLoadIdentity()
	gl.glTranslatef(0,0,-2)
	gl.glRotatef(aa.w, aa.x, aa.y, aa.z)
	gl.glBegin(gl.GL_LINES)
	gl.glColor3f(1,0,0) gl.glVertex3f(0,0,0) gl.glVertex3f(1,0,0)
	gl.glColor3f(0,1,0) gl.glVertex3f(0,0,0) gl.glVertex3f(0,1,0)
	gl.glColor3f(0,0,1) gl.glVertex3f(0,0,0) gl.glVertex3f(0,0,1)
	gl.glEnd()
	gl.glDepthMask(gl.GL_TRUE)

	gl.glViewport(0, 0, self.width, self.height)
	self.view.ortho = pushOrtho
	self.view:setup(self.width / self.height)

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

	mesh:loadGL(self.shader)

	if self.drawVertexNormals then
		mesh:drawVertexNormals()
	end
	if self.drawTriNormals then
		mesh:drawTriNormals()
	end
	if self.drawTriBasis then
		mesh:drawTriBasis()
	end
	if self.useDrawPolys then
		self.shader:use()
		self.shader:setUniforms{
			useFlipTexture = self.useFlipTexture,
			useLighting = self.useLighting,
			lightDir = self.lightDir:normalize().s,
			modelViewMatrix = self.modelViewMatrix.ptr,
			projectionMatrix = self.projectionMatrix.ptr,
		}
		mesh:draw{
			-- TODO option for calculated normals?
			-- TODO shader options?
			shader = self.shader,
			beginGroup = function(g)
				if g.tex_Kd then g.tex_Kd:bind() end
				self.shader:setUniforms{
					useTextures = self.useTextures and g.tex_Kd and 1 or 0,
					--Ka = g.Ka or {0,0,0,0},	-- why are most mesh files 1,1,1,1 ambient?  because blender exports ambient as 1,1,1,1 ... but that would wash out all lighting ... smh
					Ka = {0,0,0,0},
					Kd = g.Kd and g.Kd.s or {1,1,1,1},
					Ks = g.Ks and g.Ks.s or {1,1,1,1},
					Ns = g.Ns or 10,
					-- com3 is best for closed meshes
					objCOM = mesh.com2.s,
					groupCOM = g.com2.s,
					groupExplodeDist = self.groupExplodeDist,
					triExplodeDist = self.triExplodeDist,
				}
			end,
		}
		self.shader:useNone()
	end
	if self.drawUVUnwrapEdges then
		drawUVUnwrapEdges(mesh)
	end
	if self.drawTileMeshPlaces then
		drawTileMeshPlaces(mesh)
	end
	if self.useDrawEdges then
		mesh:drawEdges(self.triExplodeDist, self.groupExplodeDist)
	end
	if self.useDrawVertexes then
		mesh:drawVertexes(self.triExplodeDist, self.groupExplodeDist)
	end
	if self.useDrawBBox then
		if not mesh.bbox then mesh:calcBBox() end
		gl.glColor3f(1,0,1)
		gl.glLineWidth(3)
		gl.glBegin(gl.GL_LINES)
		for i=0,7 do
			for j=0,2 do
				local k = bit.bxor(i, bit.lshift(1, j))
				if k > i then
					gl.glVertex3fv(mesh.bbox:corner(i).s)
					gl.glVertex3fv(mesh.bbox:corner(k).s)
				end
			end
		end
		gl.glEnd()
		gl.glLineWidth(1)
	end
	if self.hoverVtx then
		local v = mesh.vtxs.v[self.hoverVtx].pos
		if v then
			gl.glColor3f(1,0,0)
			gl.glPointSize(3)
			gl.glBegin(gl.GL_POINTS)
			gl.glVertex3fv(v.s)
			gl.glEnd()
			gl.glPointSize(1)
		end
	end

	gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_FILL)
	gl.glDisable(gl.GL_BLEND)
	gl.glDisable(gl.GL_CULL_FACE)

--[[ verify mouseray works
	local pos, dir = self:mouseRay()
	gl.glColor3f(1,1,0)
	gl.glBegin(gl.GL_POINTS)
	gl.glVertex3f((pos + dir * 10):unpack())
	gl.glEnd()
--]]

	if self.bestTriPt then
		gl.glPointSize(3)
		gl.glColor3f(1,0,0)
		gl.glBegin(gl.GL_POINTS)
		gl.glVertex3f(self.bestTriPt:unpack())
		gl.glEnd()
		gl.glPointSize(1)
	end

	App.super.update(self)

	if self.editMode == 3 then
		if self.mouse.leftPress then
			local i, bestDist = self:findClosestTriToMouse()
			local bestgroup
			if i then
				for j,g in ipairs(mesh.groups) do
					if i >= 3*g.triFirstIndex and i < 3*(g.triFirstIndex + g.triCount) then
						bestgroup = g.name
					end
				end
				print('clicked on material', bestgroup, 'tri', i, 'dist', bestDist)

				local pos, dir = self:mouseRay()
				self.bestTriPt = pos + dir * bestDist
			end
		end
	end

	if self.editMode == 1 then
		self.hoverVtx = nil
	elseif self.editMode == 2 then
		self.hoverVtx = self:findClosestVtxToMouse()
		if self.mouse.leftPress then
			self.dragVtx = self.hoverVtx
		end
	end

	require 'gl.report''here'
end

function App:mouseRay()
	if self.view.ortho then
		return self.view.pos + self.view.angle:rotate(vec3d(
			(self.mouse.pos.x*2 - 1) * self.view.orthoSize * self.width / self.height,
			(self.mouse.pos.y*2 - 1) * self.view.orthoSize,
			0	-- zero or znear?
		)),
		-self.view.angle:zAxis()
	else
		local tanFovY = math.tan(math.rad(self.view.fovY / 2))
		return
			vec3d(self.view.pos:unpack()),
			self.view.angle:rotate(vec3d(
				(self.mouse.pos.x*2 - 1) * self.width / self.height * tanFovY,
				(self.mouse.pos.y*2 - 1) * tanFovY,
				-1
			))
	end
end

function App:findClosestVtxToMouse()
	local cosEpsAngle = math.cos(math.rad(10 / self.height * self.view.fovY))
	local pos, dir = self:mouseRay()
	return self.mesh:findClosestVertexToMouseRay(
		pos,
		dir,
		-self.view.angle:zAxis(),
		cosEpsAngle)
end

function App:findClosestTriToMouse()
	local cosEpsAngle = math.cos(math.rad(10 / self.height * self.view.fovY))
	local pos, dir = self:mouseRay()
	return self.mesh:findClosestTriToMouseRay(
		pos,
		dir,
		-self.view.angle:zAxis(),
		cosEpsAngle)
end

function App:mouseDownEvent(dx, dy, shiftDown, guiDown, altDown)
	local mesh = self.mesh
	if self.editMode == 1 then
		-- orbit behavior
		App.super.mouseDownEvent(self, dx, dy, shiftDown, guiDown, altDown)
	elseif self.editMode == 2 then
		local i = self.dragVtx
		if i then
			local pos, dir = self:mouseRay()
			local dist = -self.view.angle:zAxis():dot(self.mesh.vtxs.v[i].pos - pos)
			if not shiftDown then
				local tanFovY = math.tan(math.rad(self.view.fovY / 2))
				local screenDelta = vec3d(
					(dx / self.width * 2) * self.width / self.height * tanFovY,
					(-dy / self.height * 2) * tanFovY,
					0
				)
				local vtxDelta = self.view.angle:rotate(screenDelta) * dist
				mesh.vtxs.v[i].pos = mesh.vtxs.v[i].pos + vtxDelta
			else
				mesh.vtxs.v[i].pos = mesh.vtxs.v[i].pos + self.view.angle:rotate(vec3d(0, 0, dy))
			end
			-- update in the cpu buffer if it's been generated
			if mesh.loadedGL then
				mesh.vtxBuf:updateData(ffi.sizeof'MeshVertex_t' * i + ffi.offsetof('MeshVertex_t', 'pos'), ffi.sizeof'vec3f_t', mesh.vtxs.v[i].pos.s)
			end
		end
	end
end

function App:setCenter(center)
	if not self.mesh.bbox then self.mesh:calcBBox() end	-- TODO always keep this updated?  or dirty bit?
	local size = (self.mesh.bbox.max - self.mesh.bbox.min):norm()
	self.view.orbit:set(center:unpack())
	self.view.pos = self.view.angle:zAxis() * size + self.view.orbit
end

function App:resetAngle(fwd)
	local up = vec3d(table.unpack(dirs[self.updirIndex]))
	fwd = fwd or -self.view.angle:zAxis()
	-- if 'fwd' aligns with 'up' then pick another up vector
	if math.abs(fwd:dot(up)) > .999 then
		up.x, up.y, up.z = up.z, up.x, up.y
	end
	local back = -fwd
	local right = up:cross(back)
	-- vec-ffi's quat's fromMatrix uses col-major Lua-table-of-vec3s (just like 'toMatrix')
	self.view.angle:fromMatrix{right, up, back}
--print('matrix', right, up, back)
--print('quat', self.view.angle)
	self:setCenter(self.mesh.com2)
--print('qmatrix', table.unpack(self.view.angle:toMatrix()))
-- qmatrix should match matrix if fromMatrix worked
end

function App:updateGUI()
	local mesh = self.mesh
	if ig.igBeginMainMenuBar() then
		if ig.igBeginMenu'View' then

			for _,x in ipairs{'x', 'y', 'z'} do
				ig.luatableInputFloatAsText('view pos '..x, self.view.pos, x)
			end
			for _,x in ipairs{'x', 'y', 'z', 'w'} do
				ig.luatableInputFloatAsText('view angle '..x, self.view.angle, x)
			end
			ig.luatableInputFloat('view znear', self.view, 'znear')
			ig.luatableInputFloat('view zfar', self.view, 'zfar')
			ig.luatableCheckbox('ortho view', self.view, 'ortho')

			for i,name in ipairs(dirnames) do
				if ig.luatableRadioButton('up '..name, self, 'updirIndex', i) then
					self:resetAngle()
				end
			end
			for i,name in ipairs(dirnames) do
				if ig.igButton('reset view '..name) then
					self:resetAngle(vec3d(table.unpack(dirs[i])))
				end
			end

			if ig.igButton'set to origin' then
				self:setCenter(vec3f(0,0,0))
			end
			if ig.igButton'set to vtx center' then
				self:setCenter(mesh.com0)
			end
			if ig.igButton'set to line center' then
				self:setCenter(mesh.com1)
			end
			if ig.igButton'set to face center' then
				self:setCenter(mesh.com2)
			end
			if ig.igButton'set to volume center' then
				self:setCenter(mesh.com3)
			end
			ig.igEndMenu()
		end
		if ig.igBeginMenu'Mesh' then
			self.translate = self.translate or table{1,1,1}
			ig.luatableInputFloat('translate x', self.translate, 1)
			ig.luatableInputFloat('translate y', self.translate, 2)
			ig.luatableInputFloat('translate z', self.translate, 3)
			if ig.igButton'translate' then
				mesh:translate(self.translate:unpack())
			end

			if ig.igButton'recenter com0' then
				mesh:recenter(mesh.com0)
			end
			if ig.igButton'recenter com1' then
				mesh:recenter(mesh.com1)
			end
			if ig.igButton'recenter com2' then
				mesh:recenter(mesh.com2)
			end
			if ig.igButton'recenter com3' then
				mesh:recenter(mesh.com3)
			end

			self.scale = self.scale or table{1,1,1}
			ig.luatableInputFloat('scale x', self.scale, 1)
			ig.luatableInputFloat('scale y', self.scale, 2)
			ig.luatableInputFloat('scale z', self.scale, 3)
			if ig.igButton'scale' then
				mesh:scale(self.scale:unpack())
			end

			if ig.igButton'gen. vertex normals' then
				mesh:generateVertexNormals()
			end
			if ig.igButton'clear vertex normals' then
				mesh:clearVertexNormals()
			end
			if ig.igButton'merge vertexes' then
				mesh:mergeMatchingVertexes()
			end
			if ig.igButton'merge vertexes w/o t.c.' then
				-- there's another flag for 'skip normals' but you can just clear them so
				mesh:mergeMatchingVertexes(true)
			end
			if ig.igButton'break triangles' then
				mesh:breakTriangles()
			end

			-- triangles
			if ig.igButton'gen. tri basis' then
				mesh:generateTriBasis()
			end
			if ig.igButton'clear tri basis' then
				mesh:clearTriBasis()
			end

			ig.igEndMenu()
		end
		if ig.igBeginMenu'UV' then
			ig.luatableInputFloat('unwrap angle threshold', self, 'unwrapAngleThreshold')

			if ig.igButton'unwrap uvs' then
				timer('unwrapping uvs', function()
					unwrapUVs{
						mesh = self.mesh,
						angleThreshold = self.unwrapAngleThreshold,
					}
				end)
				if mesh.loadedGL then
					mesh.vtxBuf:updateData(0, ffi.sizeof'MeshVertex_t' * mesh.vtxs.size, mesh.vtxs.v)
				end
			end

			ig.luatableCheckbox('draw uv unwrap edges', self, 'drawUVUnwrapEdges')

			self.tileMeshFilename = self.tileMeshFilename or ''
			ig.luatableInputText('tile mesh filename', self, 'tileMeshFilename')
			if ig.igButton'tile mesh' then
				tileMesh(mesh, OBJLoader():load(self.tileMeshFilename))
			end

			ig.igEndMenu()
		end
		if ig.igBeginMenu'Display' then
			ig.luatableCheckbox('use cull face', self, 'useCullFace')
			ig.luatableCheckbox('use depth test', self, 'useDepthTest')
			ig.luatableCheckbox('use blend', self, 'useBlend')
			ig.luatableCheckbox('use textures', self, 'useTextures')
			ig.luatableCheckbox('flip texture', self, 'useFlipTexture')
			if ig.luatableCheckbox('nearest filter', self, 'useTexFilterNearest') then
				for _,g in ipairs(mesh.groups) do
					if g.tex_Kd then
						g.tex_Kd:bind()
						g.tex_Kd:setParameter(gl.GL_TEXTURE_MAG_FILTER, self.useTexFilterNearest and gl.GL_NEAREST or gl.GL_LINEAR)
						g.tex_Kd:unbind()
					end
				end
			end
			ig.luatableCheckbox('use lighting', self, 'useLighting')

			-- TODO max dependent on bounding radius of model, same with COM camera positioning
			-- TODO per-tri exploding as well
			ig.luatableSliderFloat('group explode dist', self, 'groupExplodeDist', 0, 1)
			ig.luatableSliderFloat('tri explode dist', self, 'triExplodeDist', 0, 1)
			ig.luatableCheckbox('draw bbox', self, 'useDrawBBox')
			ig.luatableCheckbox('wireframe', self, 'useWireframe')
			ig.luatableCheckbox('draw vertexes', self, 'useDrawVertexes')
			ig.luatableCheckbox('draw edges', self, 'useDrawEdges')
			ig.luatableCheckbox('draw polys', self, 'useDrawPolys')
			ig.luatableCheckbox('draw vertex normals', self, 'drawVertexNormals')
			ig.luatableCheckbox('draw tri normals', self, 'drawTriNormals')
			ig.luatableCheckbox('draw tri basis', self, 'drawTriBasis')
			ig.luatableCheckbox('draw tile placement locations', self, 'drawTileMeshPlaces')

			ig.igEndMenu()
		end
		if ig.igBeginMenu'Edit' then
			ig.luatableRadioButton('rotate mode', self, 'editMode', 1)
			ig.luatableRadioButton('edit vertex mode', self, 'editMode', 2)
			ig.luatableRadioButton('edit tri mode', self, 'editMode', 3)

			ig.igEndMenu()
		end
		if ig.igBeginMenu'Settings' then
			ig.igColorPicker3('background color', self.bgcolor.s, 0)

			ig.igEndMenu()
		end
		ig.igEndMainMenuBar()
	end
end

App():run()
