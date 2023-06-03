#!/usr/bin/env luajit
local OBJLoader = require 'mesh.objloader'
local cmdline = require 'ext.cmdline'(...)
local file = require 'ext.file'

local meshfn, dotfn = ...
assert(meshfn and dotfn, "expected mesh dot")
local mesh = OBJLoader():load(meshfn)

-- target_basic-bricks ...
-- as-is the mesh graph is fine
-- merging vertexes, there's one bad edge
-- then splitting and merging, there's a bunch
mesh:mergeMatchingVertexes(true, true)
mesh:removeEmptyTris()
if cmdline.mergevtxs then
	mesh:splitVtxsTouchingEdges()
	mesh:mergeMatchingVertexes(true, true)
	mesh:removeEmptyTris()
end	
mesh:findEdges()

-- ok here ... I need to remove tris to edges with 3 tris touching them
dotfn = file(dotfn):open'w'
dotfn:write'graph G {\n'
local edgeIndex = 1
for a,o in pairs(mesh.edges) do
	for b,e in pairs(o) do
		-- make a graph between all edges
		if #e.tris > 2 then
			print('found bad edges ',a,', ',b)
			dotfn:write('"e'..edgeIndex..'" [shape=circle, style=filled, fillcolor=red]\n')
		end
		for _,t in ipairs(e.tris) do
			dotfn:write('"e'..edgeIndex..'" -- "t'..t.index..'"\n')
		end
		edgeIndex = edgeIndex + 1
	end
end
dotfn:write'}\n'
dotfn:close()

-- neato -Tsvg dotfn > dotfn:gsub('dot', 'svg')
