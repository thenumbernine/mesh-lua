#!/usr/bin/env luajit
local timer = require 'ext.timer'
local table = require 'ext.table'

local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")
local loader = require 'mesh.objloader'()

-- use quotes around the args ...
local keepers = table{...}:sub(3):mapi(function(x)
	return true, x
end):setmetatable(nil)
print('keeping '..require'ext.tolua'(keepers))

local mesh = loader:load(infn)
print('mesh has '..(mesh.triIndexBuf.size/3)..' triangles')
for mtlname in pairs(keepers) do
	print(mtlname, mesh:getTriIndexesForMaterial(mtlname))
end

timer('filtering faces', function()
	for _,mtlname in ipairs(table.keys(mesh.mtllib):sort()) do
		local mtl = mesh.mtllib[mtlname]
		if keepers[mtlname] then
			print('keeping material '..mtlname)
		else
			if mtlname == '' then
				mesh.mtllib[mtlname].triFirstIndex = 0
				mesh.mtllib[mtlname].triCount = 0
			else
				mesh.mtllib[mtlname] = nil
			end
		end
	end
end)

-- save will remove the rest
timer('saving', function()
	loader:save(outfn, mesh)
end)
