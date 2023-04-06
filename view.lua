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
	self:setCenterD2()
	gl.glEnable(gl.GL_DEPTH_TEST)
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
end

function App:deleteDisplayList()
	if self.displayList.id then
		gl.glDeleteLists(self.displayList.id, 1)
		self.displayList.id = nil
	end
end

-- center by vtx avg
function App:setCenterD0()
	local obj = self.obj
	self:setCenter(obj.vs:sum() / #obj.vs)
end

-- center by edge avg
function App:setCenterD1()
	-- TODO this upon load
	local obj = self.obj
	local edges = {}
	local function addEdge(a,b)
		if a > b then return addEdge(b,a) end
		edges[a] = edges[a] or {}
		edges[a][b] = true
	end
	local function addTri(a,b,c)
		addEdge(a,b)
		addEdge(a,c)
		addEdge(b,c)
	end
	for a,b,c in obj:triiter() do
		addEdge(a.v,b.v)
		addEdge(a.v,c.v)
		addEdge(b.v,c.v)
	end
	local totalCOM = vec3()
	local totalArea = 0
	for a,bs in pairs(edges) do
		for b in pairs(bs) do
			local v1 = obj.vs[a]
			local v2 = obj.vs[b]
			local area = (v1 - v2):length()
			local com = (v1 + v2) * .5
			totalCOM = totalCOM + com * area
			totalArea = totalArea + area
		end
	end
	self:setCenter(totalCOM / totalArea)
end

function App:setCenterD2()
	local obj = self.obj
	local totalCOM = vec3()
	local totalArea = 0
	for i,j,k in obj:triiter() do
		local a = obj.vs[i.v]
		local b = obj.vs[j.v]
		local c = obj.vs[k.v]
		local ab = b - a
		local ac = c - a
		local area = ab:cross(ac):length() * .5
		local com = (a + b + c) * (1/3)
		totalCOM = totalCOM + com * area
		totalArea = totalArea + area
	end
	self:setCenter(totalCOM / totalArea)
end

function App:setCenter(center)
	local size = self.obj.vs:mapi(function(v) return (v - center):length() end):sup()
	self.view.orbit:set(center:unpack())
	self.view.pos = self.view.orbit + self.view.angle:zAxis() * size
end

function App:updateGUI()
	if ig.igButton'set to vtx center' then
		self:setCenterD0()
	end
	if ig.igButton'set to line center' then
		self:setCenterD1()
	end
	if ig.igButton'set to face center' then
		self:setCenterD2()
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
