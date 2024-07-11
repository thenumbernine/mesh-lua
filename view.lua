#!/usr/bin/env luajit
local ffi = require 'ffi'
local path = require 'ext.path'
local table = require 'ext.table'
local math = require 'ext.math'
local timer = require 'ext.timer'
local op = require 'ext.op'
local sdl = require 'ffi.req' 'sdl'
local gl = require 'gl'
local GLSceneObject = require 'gl.sceneobject'
local ig = require 'imgui'
local vec3f = require 'vec-ffi.vec3f'
local vec3d = require 'vec-ffi.vec3d'
local vec4f = require 'vec-ffi.vec4f'
local plane3f = require 'vec-ffi.plane3f'
local quatd = require 'vec-ffi.quatd'
local cmdline = require 'ext.cmdline'(...)
local Mesh = require 'mesh'
local OBJLoader = require 'mesh.objloader'
local unwrapUVs = require 'mesh.unwrapuvs'.unwrapUVs
local drawUnwrapUVGraph = require 'mesh.unwrapuvs'.drawUnwrapUVGraph
local drawUnwrapUVEdges = require 'mesh.unwrapuvs'.drawUnwrapUVEdges
local tileMesh = require 'mesh.tilemesh'.tileMesh
local drawTileMeshPlaces = require 'mesh.tilemesh'.drawTileMeshPlaces
local matrix3x3To4x4 = require 'mesh.common'.matrix3x3To4x4
local translateMat4x4 = require 'mesh.common'.translateMat4x4

local fn = cmdline.file
if not fn then fn = ... end
if not fn then error("can't figure out what your file is from the cmdline") end

local App = require 'imguiapp.withorbit'{
	viewUseBuiltinMatrixMath = true,
}

App.title = 'WavefrontOBJ preview'

local editModeNames = table{
	'rotate',
	'vertex',
	'tri',
	'edge',
	'insertMeshToSurfaceClipGroup',
	'insertMeshToEdgeClipGroup',
}
local editModeForName = editModeNames:mapi(function(v,k) return k,v end)

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

local function default(a, b)
	if a == nil then return b end
	return a
end

function App:initGL(...)
	App.super.initGL(self, ...)

	assert(self.view.useBuiltinMatrixMath)

	self.view.znear = .1
	self.view.zfar = 40000

	-- gui options
	self.useWireframe = false
	self.useDrawVertexes = false
	self.useDrawBBox = false
	self.useDrawEdges = false
	self.useDrawPolys = default(cmdline.drawPolys, true)
	self.drawVertexNormals = false
	self.drawTriNormals = false
	self.drawTriBasis = false
	self.useTextures = true
	self.useFlipTexture = false	-- opengl vs directx? v=0 is bottom or top?
	self.useTexFilterNearest = false

	self.drawUnwrapUVGraph = false
	self.drawUnwrapUVEdges = false
	self.drawTileMeshPlaces = false
	self.drawTriSurfaceGroupEdges = default(cmdline.drawTriSurfaceGroupEdges, false)
	self.drawTriSurfaceGroupPlanes = false
	self.drawTriGroupEdgeClipPlanes = default(cmdline.drawTriGroupEdgeClipPlanes, false)

	self.editMode = editModeForName.rotate

	self.useLighting = false
	self.lightDir = vec3f(1,1,1)

	self.useCullFace = default(cmdline.cull, true)
	self.useDepthTest = true
	self.useBlend = true
	self.useAlphaTest = true
	self.groupExplodeDist = 0
	self.triExplodeDist = 0
	--self.bgcolor = vec4f(.2, .3, .5, 1)
	self.bgcolor = vec4f(0,0,0,1)


	self.curfn = fn
	local curname
	self.curdir, curname = path(fn):getdir()
	sdl.SDL_SetWindowTitle(self.window, self.title..': '..curname)
	local mesh = OBJLoader():load(self.curfn)

	-- TODO how to request this?  dirty bits?
	mesh:prepare()

	if cmdline.tribasis then
		mesh:generateTriBasis()
		self.drawTriBasis = true
	end

print('#unique vertexes', #mesh.vtxs)
print('#unique triangles', #mesh.triIndexes/3)

	-- TODO make this an option with specified threshold.
	-- calcBBox has to be done first
	-- after doing this you have to call findEdges and calcCOMs
	if cmdline.mergevtxs then
		timer('merging vertexes', function()
			-- merge vtxs with vtxs ... ignoring texcoords and normals
			mesh:mergeMatchingVertexes(true, true)
			mesh:removeEmptyTris()

print('#unique vertexes', #mesh.vtxs)
--[[
for i=0,#mesh.vtxs-1 do
	print(('%f\t%f\t%f'):format(mesh.vtxs.v[i].pos:unpack()))
end
--]]
print('#unique triangles', #mesh.triIndexes/3)
--[[
for i=0,#mesh.triIndexes-3,3 do
	local tp = mesh.triIndexes.v + i
	print(tp[0], tp[1], tp[2])
end
--]]

			-- if two tris touch, or almost touch, then split them along the edge in common with their planes
			-- maybe I don't have to do this yet ...
			--mesh:splitTrisTouchingTris()
			-- merge vtxs with edges - i.e. split any edges where a vertex is overlapping it midway
			mesh:splitVtxsTouchingEdges()
			-- need these two calls or the edge-find doesn't find proper boundaries to target_complex
			mesh:mergeMatchingVertexes(true, true)
			mesh:removeEmptyTris()

			-- merge before delaunay ...
			-- or will that screw up edges that might be used elsewhere?
			-- TODO try to get by without doing this
			--mesh:mergeMatchingVertexes(true, true)
			--mesh:delaunayTriangulate()
		end)
		-- refresh edges, com0, and com1
		mesh:findEdges()
		mesh.com0 = mesh:calcCOM0()
		mesh.com1 = mesh:calcCOM1()
	end

--print('area '..table(mesh.tris):mapi(function(t) return t.area end):sort():reverse():concat'\narea '..'\n')

	-- TODO give every vtx a TNB, use it instead of uvbasis3D, and don't have tilemesh require unwrapuv
	if cmdline.unwrapuv then
		-- calculate unique volumes / calculate any distinct pieces on them not part of the volume
		-- unwrapUVs requires an angle threshold of 5 deg or so ... the default in Mesh right now
		timer('unwrapping uvs', function()
			unwrapUVs(mesh)
		end)
	end
	if cmdline.tilemesh or cmdline.tilemeshmerge then
		-- tile omesh onto mesh in-place
		local srcmesh
		if cmdline.tilemeshmerge then
			srcmesh = mesh:clone()
		end
		tileMesh(mesh, cmdline.tilemesh or cmdline.tilemeshmerge)
		if cmdline.tilemeshmerge then
			mesh:combine(srcmesh)
		end
	end
--]]

	print('triangle bounded volume', mesh:calcVolume())
	print('bbox', mesh.bbox)
	print('bbox size', mesh.bbox.max - mesh.bbox.min)
	print('bbox volume', (mesh.bbox.max - mesh.bbox.min):volume())
	print('mesh.bbox corner-to-corner distance: '..(mesh.bbox.max - mesh.bbox.min):norm())
	self.mesh = mesh

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

	self.shader = Mesh:makeShader()

	self.basisView = require 'glapp.view'{
		useBuiltinMatrixMath = true,
		pos = {0, 0, 2},
	}

	self.basisObj = GLSceneObject{
		program = {
			version = 'latest',
			vertexCode = [[
in vec3 vtx;
in vec3 color;
out vec3 colorv;
uniform mat4 mvMat, projMat;
void main() {
	colorv = color;
	gl_Position = projMat * (mvMat * vec4(vtx, 1.));
}
]],
			fragmentCode = [[
in vec3 colorv;
out vec4 fragColor;
void main() {
	fragColor = vec4(colorv, 1.);
}
]],
		},
		geometry = {
			mode = gl.GL_LINES,
			-- TODO infer this from attr buffer size / dim ... ?
			-- though gl don't care about a buffer's element count or dimension, so gl.buffer doesn't store it
			count = 6,
		},
		attrs = {
			vtx = {	-- GLAttribue
				buffer = {	-- GLArrayBuffer
					data = {
						0, 0, 0,
						1, 0, 0,
						0, 0, 0,
						0, 1, 0,
						0, 0, 0,
						0, 0, 1,
					},
				},
			},
			color = {	-- GLAttribue
				buffer = {	-- GLArrayBuffer
					data = {
						1, 0, 0,
						1, 0, 0,
						0, 1, 0,
						0, 1, 0,
						0, 0, 1,
						0, 0, 1,
					},
				},
			},
		},
	}

	mesh:loadGL(self.shader)
end

function App:update()
	local mesh = self.mesh

	gl.glClearColor(self.bgcolor:unpack())
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))

	gl.glDepthFunc(gl.GL_LEQUAL)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)

	do
		gl.glDepthMask(gl.GL_FALSE)
		local axisw = math.ceil(math.min(self.width, self.height) / 4)
		gl.glViewport(self.width-1-axisw, self.height-1-axisw, axisw, axisw)
		self.basisView.angle:set(self.view.angle:unpack())
		self.basisView.pos:set((self.basisView.angle:zAxis() * 2):unpack())
		self.basisView:setup(1)
		self.basisObj:draw{
			uniforms = {
				mvMat = self.basisView.mvMat.ptr,
				projMat = self.basisView.projMat.ptr,
			},
		}
		gl.glDepthMask(gl.GL_TRUE)

		gl.glViewport(0, 0, self.width, self.height)
	end

	if self.useDepthTest then
		gl.glEnable(gl.GL_DEPTH_TEST)
	else
		gl.glDisable(gl.GL_DEPTH_TEST)
	end
	if self.useAlphaTest then
		if op.safeindex(gl, 'GL_ALPHA_TEST') then
			gl.glEnable(gl.GL_ALPHA_TEST) 
			gl.glAlphaFunc(gl.GL_GEQUAL, 0.5)
		end
	end
	if self.useBlend then
		gl.glEnable(gl.GL_BLEND)
	end
	if self.useCullFace then
		--gl.glFrontFace(gl.GL_CCW)
		--gl.glCullFace(gl.GL_BACK)
		gl.glEnable(gl.GL_CULL_FACE)
	end
	if self.useWireframe 
	and op.safeindex(gl, 'glPolygonMode')
	then
		gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_LINE)
	end

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
	do
		self.shader:use()
		self.shader:setUniforms{
			useFlipTexture = self.useFlipTexture,
			useLighting = self.useLighting,
			lightDir = self.lightDir:normalize().s,
			mvMat = self.view.mvMat.ptr,
			projMat = self.view.projMat.ptr,
		}
		if self.useDrawPolys then
			self:drawMesh(mesh)
		end
		if self.insertMeshToSurfaceClipGroup then
			self:drawMesh(self.insertMeshToSurfaceClipGroup)
		end
		if self.insertMeshToEdgeClipGroup then
			self:drawMesh(self.insertMeshToEdgeClipGroup)
			gl.glPointSize(5)
			gl.glColor3f(1,1,0)
			gl.glBegin(gl.GL_POINTS)
			gl.glVertex3f(self.insertMeshPt:unpack())
			gl.glEnd()
			gl.glPointSize(1)
		end
		self.shader:useNone()
	end
	if self.useBlend then
		gl.glDisable(gl.GL_BLEND)
	end
	if self.useAlphaTest then
		if op.safeindex(gl, 'GL_ALPHA_TEST') then gl.glDisable(gl.GL_ALPHA_TEST) end
	end

	if self.drawUnwrapUVGraph then
		drawUnwrapUVGraph(mesh)
	end
	if self.drawUnwrapUVEdges then
		drawUnwrapUVEdges(mesh)
	end
	if self.drawTileMeshPlaces then
		drawTileMeshPlaces(mesh)
	end
	if self.drawTriSurfaceGroupPlanes then
		mesh:drawTriSurfaceGroupPlanes()
	end
	if self.drawTriGroupEdgeClipPlanes then
		mesh:drawTriGroupEdgeClipPlanes(self.hoverEdge)
	end
	if self.drawTriSurfaceGroupEdges then
		mesh:drawTriSurfaceGroupEdges(self.hoverEdge)
	end
	if self.useDrawEdges then
		mesh:drawEdges(self.triExplodeDist, self.groupExplodeDist)
	end
	if self.useDrawVertexes then
		mesh:drawVertexes(self.triExplodeDist, self.groupExplodeDist)
	end
	if self.debugDrawLoops then
		gl.glColor3f(0,1,1)
		gl.glLineWidth(3)
		for _,loop in ipairs(self.debugDrawLoops) do
			gl.glBegin(gl.GL_LINE_LOOP)
			for _,l in ipairs(loop) do
				gl.glVertex3f(mesh:getPosForLoopChain(l):unpack())
			end
			gl.glEnd()
		end
		gl.glLineWidth(1)
	end
	if self.debugDrawLines then
		gl.glColor3f(1,0,0)
		gl.glLineWidth(3)
		for _,line in ipairs(self.debugDrawLines) do
			gl.glBegin(gl.GL_LINE_STRIP)
			for _,l in ipairs(line) do
				gl.glVertex3f(mesh:getPosForLoopChain(l):unpack())
			end
			gl.glEnd()
		end
		gl.glLineWidth(1)
	end
	if self.debugDrawBadEdges then
		gl.glColor3f(1,0,1)
		gl.glLineWidth(3)
		gl.glBegin(gl.GL_LINES)
		for _,i in ipairs(self.debugDrawBadEdges) do
			gl.glVertex3f(mesh.vtxs.v[i].pos:unpack())
		end
		gl.glEnd()
		gl.glLineWidth(1)
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

	if op.safeindex(gl, 'glPolygonMode') then
		gl.glPolygonMode(gl.GL_FRONT_AND_BACK, gl.GL_FILL)
	end
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
	if self.bestEdgePt then
		gl.glPointSize(5)
		gl.glColor3f(1,1,0)
		gl.glBegin(gl.GL_POINTS)
		gl.glVertex3f(self.bestEdgePt:unpack())
		gl.glEnd()
		gl.glPointSize(1)
	end

	App.super.update(self)

	if self.editMode == editModeForName.rotate then
		self.hoverVtx = nil
		self.hoverTri = nil
		self.bestTriPt = nil
		self.hoverEdge = nil
		self.bestEdgePt = nil
	elseif self.editMode == editModeForName.vertex then
		self.hoverVtx = self:findClosestVtxToMouse()
		if self.mouse.leftPress then
			self.dragVtx = self.hoverVtx
		end
	elseif self.editMode == editModeForName.tri then
		self.hoverTri, self.bestTriPt = self:findClosestTriToMouse()
		if not self.hoverTri then self.bestTriPt = nil end
		if self.mouse.leftPress then
			-- [[
			local bestgroup
			if self.hoverTri then
				for j,g in ipairs(mesh.groups) do
					if self.hoverTri >= 3*g.triFirstIndex and self.hoverTri < 3*(g.triFirstIndex + g.triCount) then
						bestgroup = g.name
					end
				end
				print('clicked on material', bestgroup, 'tri', self.hoverTri/3)
			end
			--]]
			self.dragTri = self.hoverTri
		end
	elseif self.editMode == editModeForName.edge then
		self.hoverEdge, self.bestEdgePt = self:findClosestTriGroupEdgeToMouse()
		if not self.hoverEdge then self.bestEdgePt = nil end
		if self.mouse.leftPress then
			if self.hoverEdge then
				print('clicking on edge', self.hoverEdge)
				if not mesh.edgeClipGroups then
					mesh:calcTriEdgeGroups()
				end
				local _, eg = mesh.edgeClipGroups:find(nil, function(eg)
					return eg.srcEdges:find(nil, function(es)
						return es.edge == self.hoverEdge
					end)
				end)
				print('clicking edge group', eg)
				if eg then
					for i,es in ipairs(eg.srcEdges) do
						local e = es.edge
						print('src edge #'..i..' has intervalIndex '..es.intervalIndex)
						print('points', e:getPts())
						print('interval', table.unpack(e.interval))
						print('length', e.interval[2] - e.interval[1])
						print('plane', e.plane)
						print('planePos', e.planePos)
						print('clipPlane', e.clipPlane)
						print('normAvg', e.normAvg)
						print('com', e.com)
					end
				end
			end
			self.dragEdge = self.hoverEdge
		end
	elseif self.editMode == editModeForName.insertMeshToSurfaceClipGroup then
		self.insertMeshTri, self.insertMeshPt = self:findClosestTriToMouse()
		if not self.insertMeshTri then
			self.insertMeshPt = nil
			self.insertMeshToSurfaceClipGroupBase = nil
			self.insertMeshToSurfaceClipGroup = nil
		else
			if not self.insertMeshToSurfaceClipGroupBase then
				if self.insertMeshFilename ~= '' then
--print('loading', self.insertMeshFilename)
					xpcall(function()
--print('self.insertMeshToSurfaceClipGroupBase')
						self.insertMeshToSurfaceClipGroupBase = OBJLoader():load(self.insertMeshFilename)
--print('self.insertMeshToSurfaceClipGroupBase')
						self.insertMeshToSurfaceClipGroupBase:findEdges()
--print('self.insertMeshToSurfaceClipGroupBase')
						self.insertMeshToSurfaceClipGroupBase:calcCOMs()
					end, function(err)
						print(err..'\n'..debug.traceback())
					end)
				end
			else
				if self.insertMeshToSurfaceClipGroup then
					self.insertMeshToSurfaceClipGroup:unloadGL()
				end
				self.insertMeshToSurfaceClipGroup = self.insertMeshToSurfaceClipGroupBase:clone()
				self.insertMeshToSurfaceClipGroup:translate(self.insertMeshPt:unpack())

				if mesh.triGroups then
					local tg = mesh.triGroupForTri[mesh.tris[self.insertMeshTri/3+1]]
					if tg then
						self.insertMeshToSurfaceClipGroup = self.insertMeshToSurfaceClipGroup:clipToClipGroup(tg)
					end
				end
				if #self.insertMeshToSurfaceClipGroup.tris == 0 then
					self.insertMeshToSurfaceClipGroup = nil
				end
				if self.insertMeshToSurfaceClipGroup then
					self.insertMeshToSurfaceClipGroup:loadGL(self.shader)
				end
			end
		end
	elseif self.editMode == editModeForName.insertMeshToEdgeClipGroup then
--print('finding closest edge to mouseray...')
		local insertMeshEdge
		insertMeshEdge, self.insertMeshPt = self:findClosestTriGroupEdgeToMouse()
--print('insertMeshEdge', insertMeshEdge)
--print('self.insertMeshPt', self.insertMeshPt)
		if not insertMeshEdge then
--print('insertMeshEdge exists - clearing all edge placement vars')
			self.insertMeshPt = nil
			self.insertMeshToEdgeClipGroupBase = nil
			self.insertMeshToEdgeClipGroup = nil
		else
--print('insertMeshEdge exists...')
--print('self.insertMeshToEdgeClipGroupBase', self.insertMeshToEdgeClipGroupBase)
			if not self.insertMeshToEdgeClipGroupBase then
--print("self.insertMeshToEdgeClipGroupBase doesn't exist - loading")
--print('self.insertMeshFilename', self.insertMeshFilename)
				if self.insertMeshFilename ~= '' then
--print('loading...')
					xpcall(function()
--print('self.insertMeshToSurfaceClipGroupBase')
						self.insertMeshToEdgeClipGroupBase = OBJLoader():load(self.insertMeshFilename)
--print('self.insertMeshToEdgeClipGroupBase')
						self.insertMeshToEdgeClipGroupBase:findEdges()
--print('self.insertMeshToEdgeClipGroupBase')
						self.insertMeshToEdgeClipGroupBase:calcCOMs()
					end, function(err)
						print(tostring(err)..'\n'..debug.traceback())
					end)
				end
			else
--print("self.insertMeshToEdgeClipGroupBase does exist...")
--print("self.insertMeshToEdgeClipGroup", self.insertMeshToEdgeClipGroup)
				if self.insertMeshToEdgeClipGroup then
--print("self.insertMeshToEdgeClipGroup exists ... unloading associated GL objects")
					self.insertMeshToEdgeClipGroup:unloadGL()
--print("self.insertMeshToEdgeClipGroup exists ... done unloading associated GL objects")
				end

--print('calculating basis of insertMeshEdge')
				local ey = insertMeshEdge.normAvg
				local ez = -insertMeshEdge.plane.n
				local ex = ey:cross(ez)
--print('got', ex, ey, ez)

--print('cloning and transforming...')
				self.insertMeshToEdgeClipGroup = self.insertMeshToEdgeClipGroupBase:clone()
				self.insertMeshToEdgeClipGroup:transform(
					translateMat4x4(self.insertMeshPt)
					* matrix3x3To4x4{ex, ey, ez}
				)
--print('done cloning and transforming...')

--print('mesh.edgeClipGroups', mesh.edgeClipGroups)
				if not mesh.edgeClipGroups then
					mesh:calcTriEdgeGroups()
				end
				assert(mesh.edgeClipGroups)
--print("... doesn't exist - can't clip anything")
--print("looking for closest edge to mouse")
				local _, eg = mesh.edgeClipGroups:find(nil, function(eg)
					return eg.srcEdges:find(nil, function(es)
						return es.edge == insertMeshEdge
					end)
				end)
--print("found", eg)
				if eg then
					self.insertMeshToEdgeClipGroup = self.insertMeshToEdgeClipGroup:clipToClipGroup(eg)
				end
				if self.insertMeshToEdgeClipGroup
				and #self.insertMeshToEdgeClipGroup.tris == 0
				then
--print("no tris after clip - clearing model")
					self.insertMeshToEdgeClipGroup = nil
				end
				if self.insertMeshToEdgeClipGroup then
--print("loading GL stuff...")
					self.insertMeshToEdgeClipGroup:loadGL(self.shader)
				end
			end
		end
	end

	require 'gl.report''here'
end

function App:drawMesh(mesh)
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
	local triIndex, bestDist = self.mesh:findClosestTriToMouseRay(
		pos,
		dir,
		-self.view.angle:zAxis(),
		cosEpsAngle)
	if triIndex then
		return triIndex, pos + dir * bestDist
	end
end

--[[
intersects a+s*b with c+t*d
returns s,t
if they're parallel returns nil
--]]
--[[
|(a + s b) - (c + t d)| is minimal wrt (s,t)
(a + s b).(a + s b) - (a + s b).(c + t d) - (c + t d).(a + s b) + (c + t d).(c + t d) is minimal wrt (s,t)
(
	+ a.a
	+ 2 s a.b
	+ s^2 b.b

	- 2 a.c
	- 2 s b.c
	- 2 t a.d
	- 2 s t b.d

	+ c.c
	+ 2 t c.d
	+ t^2 d.d
) is minimal wrt (s,t)
...which is where [d/ds, d/dt] = [0,0]

d/ds = 2 (
	a.b - c.b
	+ s b.b
	- t d.b
)

d/dt = -2 (
	a.d - c.d
	+ s b.d
	- t d.d
)

[(c-a).b]   [b.b -b.d] [s]
[(c-a).d] = [b.d -d.d] [t]

[s]   [b.b -b.d]^-1 [(c-a).b]
[t] = [b.d -d.d]    [(c-a).d]

det = b.d^2 - b.b d.d

[s]   [-d.d b.d] [(c-a).b]
[t] = [-b.d b.b] [(c-a).d] / det

s = (-d.d (c-a).b + b.d (c-a).d) / det
t = (-b.d (c-a).b + b.b (c-a).d) / det

s = d.(b (c-a).d - d (c-a).b) / det
t = b.(b (c-a).d - d (c-a).b) / det
--]]
function rayRayIntersect(a,b,c,d)
--print('e', e, 's', s0, s1)
	local ac = c - a
	local b_dot_d = b:dot(d)
	local b_dot_b = b:lenSq()
	local d_dot_d = d:lenSq()
	local detA = b_dot_d*b_dot_d - b_dot_b*d_dot_d
--print('detA', detA)
	if math.abs(detA) < 1e-7 then
		return nil, "rays are parallel"
	end
	local invDetA = 1/detA
	local s = (-d_dot_d * ac:dot(b) + b_dot_d * ac:dot(d)) * invDetA
	local t = (-b_dot_d * ac:dot(b) + b_dot_b * ac:dot(d)) * invDetA
--print('s', s, 't', t)
	return s, t
end

function App:findClosestTriGroupEdgeToMouse()
--print('App:findClosestTriGroupEdgeToMouse()')
	local mousePos, mouseDir = self:mouseRay()
--print('mousePos', mousePos, 'mouseDir', mouseDir)
	local bestDistSq = math.huge
	local bestEdge, bestPt1, bestPt2
	local mesh = self.mesh
	if not mesh.triGroups then
		mesh:calcTriSurfaceGroups()
	end
	for _,g in ipairs(mesh.triGroups) do
		for _,info in ipairs(g.borderEdges) do
			local e = info.edge
			local s0, s1 = table.unpack(e.interval)
			local a = mousePos
			local b = mouseDir
			local c = e.planePos
			local d = e.plane.n
			local s, t = rayRayIntersect(a,b,c,d)
			if s 		-- rays aren't parallel
			and s > 0 	-- front facing the camera
			then
				-- clamp to edge line segment parameter length
				t = math.clamp(t, s0, s1)
				local p1 = a + b * s
				local p2 = c + d * t
				local distSq = (p2 - p1):lenSq()
				if distSq < bestDistSq then
					bestDistSq = distSq
					bestPt1 = p1	-- on the mouse ray
					bestPt2 = p2	-- on the line segment
					bestEdge = e
				end
			end
		end
	end
	if bestEdge then
		return bestEdge, bestPt2, bestPt1
	end
end

function App:mouseDownEvent(dx, dy, shiftDown, guiDown, altDown)
	local mesh = self.mesh
	local pos, dir	-- calc only once per call
	local function moveVtx(i)
		if not pos then
			pos, dir = self:mouseRay()
		end
		local vtx = self.mesh.vtxs.v[i]
		local dist = -self.view.angle:zAxis():dot(vtx.pos - pos)
		if not shiftDown then
			local tanFovY = math.tan(math.rad(self.view.fovY / 2))
			local screenDelta = vec3d(
				(dx / self.width * 2) * self.width / self.height * tanFovY,
				(-dy / self.height * 2) * tanFovY,
				0
			)
			local vtxDelta = self.view.angle:rotate(screenDelta) * dist
			vtx.pos = vtx.pos + vtxDelta
		else
			vtx.pos = vtx.pos + self.view.angle:rotate(vec3d(0, 0, dy))
		end
		-- update in the cpu buffer if it's been generated
		if mesh.loadedGL then
			mesh.vtxBuf:updateData(ffi.sizeof'MeshVertex_t' * i + ffi.offsetof('MeshVertex_t', 'pos'), ffi.sizeof'vec3f_t', vtx.pos.s)
		end
	end
	if self.editMode == editModeForName.rotate then
		-- orbit behavior
		App.super.mouseDownEvent(self, dx, dy, shiftDown, guiDown, altDown)
	elseif self.editMode == editModeForName.vertex then
		if self.dragVtx then
			moveVtx(self.dragVtx)
		end
	elseif self.editMode == editModeForName.tri then
		if self.dragTri then
			for j=0,2 do
				moveVtx(mesh.triIndexes.v[self.dragTri + j])
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

function App:cycleFile(ofs)
	assert(self.curdir)
	if not self.curdirfiles then
		self.curdirfiles = table()
		for f in self.curdir:dir() do
			if f.path:match'%.obj$' then
				self.curdirfiles:insert(f.path)
			end
		end
		self.curdirfiles:sort()
	end
	if #self.curdirfiles == 0 then
		print("found no files in dir..?")
		return
	end
	local _,prevfn = path(self.curfn):getdir()
	local i = self.curdirfiles:find(prevfn.path)
	if not i then
		print("couldn't find current file "..tostring(self.curfn))
		i = 1
	end
	i = (i-1+ofs)%#self.curdirfiles+1
	self.curfn = self.curdir(self.curdirfiles[i]).path
	local _, curname = path(self.curfn):getdir()
print('on file '..i..' name '..self.curfn)
	sdl.SDL_SetWindowTitle(self.window, self.title..': '..curname)
	self.mesh = OBJLoader():load(self.curfn)
	self.mesh:prepare()
end

function App:updateGUI()
	local mesh = self.mesh
	if ig.igBeginMainMenuBar() then
		if ig.igBeginMenu'File' then
			--... open ...
			if ig.igButton'Prev' then
				self:cycleFile(-1)
			end
			ig.igSameLine()
			if ig.igButton'Next' then
				self:cycleFile(1)
			end
			ig.igEndMenu()
		end
		if ig.igBeginMenu'View' then

			for _,x in ipairs{'x', 'y', 'z'} do
				ig.luatableInputFloatAsText('view pos '..x, self.view.pos, x)
			end
			for _,x in ipairs{'x', 'y', 'z'} do
				ig.luatableInputFloatAsText('view orbit '..x, self.view.orbit, x)
			end
			for _,x in ipairs{'x', 'y', 'z', 'w'} do
				ig.luatableInputFloatAsText('view angle '..x, self.view.angle, x)
			end
			ig.luatableInputFloat('view znear', self.view, 'znear')
			ig.luatableInputFloat('view zfar', self.view, 'zfar')
			ig.luatableInputFloat('view fov', self.view, 'fovY')
			ig.luatableCheckbox('ortho view', self.view, 'ortho')

			ig.igText('up')
			ig.igPushID_Str('up')
			for i,name in ipairs(dirnames) do
				ig.igSameLine()
				if ig.luatableRadioButton(name, self, 'updirIndex', i) then
					self:resetAngle()
				end
			end
			ig.igPopID()

			ig.igText('reset view')
			ig.igPushID_Str('reset view')
			for i,name in ipairs(dirnames) do
				ig.igSameLine()
				if ig.igButton(name) then
					self:resetAngle(vec3d(table.unpack(dirs[i])))
				end
			end
			ig.igPopID()

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
				mesh:calcCOMs()
			end
			if ig.igButton'recenter com1' then
				mesh:recenter(mesh.com1)
				mesh:calcCOMs()
			end
			if ig.igButton'recenter com2' then
				mesh:recenter(mesh.com2)
				mesh:calcCOMs()
			end
			if ig.igButton'recenter com3' then
				mesh:recenter(mesh.com3)
				mesh:calcCOMs()
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
			if ig.igButton'break vertexes' then
				mesh:breakAllVertexes()
			end
			if ig.igButton'remove empty tris' then
				mesh:removeEmptyTris()
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
			ig.luatableInputFloat('unwrap angle threshold', mesh, 'angleThresholdInDeg')

			if ig.igButton'unwrap uvs' then
				timer('unwrapping uvs', function()
					unwrapUVs(mesh)
				end)
				if mesh.loadedGL then
					mesh.vtxBuf:updateData(0, ffi.sizeof'MeshVertex_t' * #mesh.vtxs, mesh.vtxs.v)
				end
			end

			ig.luatableCheckbox('draw uv unwrap graph', self, 'drawUnwrapUVGraph')
			ig.luatableCheckbox('draw uv unwrap edges', self, 'drawUnwrapUVEdges')

			self.tileMeshJSONFilename = self.tileMeshJSONFilename or ''
			ig.luatableInputText('placement JSON filename', self, 'tileMeshJSONFilename')
			if ig.igButton'tile mesh' then
				tileMesh(mesh, self.tileMeshJSONFilename)
			end

			ig.igEndMenu()
		end
		if ig.igBeginMenu'Display' then
			ig.luatableCheckbox('use cull face', self, 'useCullFace')
			ig.luatableCheckbox('use depth test', self, 'useDepthTest')
			ig.luatableCheckbox('use blend', self, 'useBlend')
			ig.luatableCheckbox('use alpha test', self, 'useAlphaTest')
			ig.luatableCheckbox('use textures', self, 'useTextures')
			ig.luatableCheckbox('flip texture', self, 'useFlipTexture')
			if ig.luatableCheckbox('nearest filter', self, 'useTexFilterNearest') then
				for _,g in ipairs(mesh.groups) do
					if g.tex_Kd then
						g.tex_Kd
							:bind()
							:setParameter(gl.GL_TEXTURE_MAG_FILTER, self.useTexFilterNearest and gl.GL_NEAREST or gl.GL_LINEAR)
							:unbind()
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

			ig.igSeparator()
			ig.luatableCheckbox('draw tri group edges', self, 'drawTriSurfaceGroupEdges')
			ig.luatableCheckbox('draw tri group planes', self, 'drawTriSurfaceGroupPlanes')
			ig.luatableCheckbox('draw tri group edge clip planes', self, 'drawTriGroupEdgeClipPlanes')


			ig.igSeparator()
			if ig.igButton'find holes' then
				self.debugDrawLoops, self.debugDrawLines = mesh:findBadEdges()
			end
			if ig.igButton'clear hole annotations' then
				self.debugDrawLoops, self.debugDrawLines = nil
			end
			if ig.igButton'find bad com edges' then
				if not mesh.edges2 then
					mesh:calcEdges2()
				end
				for _,e in ipairs(mesh.edges2) do
					local t1, t2 = table.unpack(e.tris)
					local t1side = e.clipPlane:test(t1.com)
					local t2side = e.clipPlane:test(t2.com)
					if t1side == t2side then
						self.debugDrawBadEdges = self.debugDrawBadEdges or table()
						self.debugDrawBadEdges:insert(3*(t1.index-1)+e.triVtxIndexes[1]-1)
						self.debugDrawBadEdges:insert(3*(t1.index-1)+e.triVtxIndexes[1]%3)
					end
				end
			end
			if ig.igButton'clear bad com edges' then
				self.debugDrawBadEdges =  nil
			end

			ig.igEndMenu()
		end
		if ig.igBeginMenu'Edit' then
			ig.luatableRadioButton('rotate mode', self, 'editMode', editModeForName.rotate)
			ig.luatableRadioButton('edit vertex mode', self, 'editMode', editModeForName.vertex)
			ig.luatableRadioButton('edit tri mode', self, 'editMode', editModeForName.tri)
			ig.luatableRadioButton('edit edge mode', self, 'editMode', editModeForName.edge)

			ig.igSeparator()
			ig.luatableRadioButton('insert mesh to surface group', self, 'editMode', editModeForName.insertMeshToSurfaceClipGroup)
			ig.luatableRadioButton('insert mesh to edge group', self, 'editMode', editModeForName.insertMeshToEdgeClipGroup)
			self.insertMeshFilename = self.insertMeshFilename or ''
			ig.luatableInputText('tri group placement filename', self, 'insertMeshFilename')

			ig.igEndMenu()
		end
		if ig.igBeginMenu'Settings' then
			ig.igColorPicker3('background color', self.bgcolor.s, 0)

			ig.igEndMenu()
		end
		ig.igEndMainMenuBar()
	end

	if self.bestTriPt then
		ig.igBeginTooltip()
		local prec = 1e-4
		ig.igText(''..self.bestTriPt:map(function(x) return math.round(x/prec)*prec end))
		ig.igEndTooltip()
	end
end

return App():run()
