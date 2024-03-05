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
print('mesh has '..(#mesh.triIndexes/3)..' triangles')
for i,groupname in ipairs(keepers) do
	print(groupname, mesh:getTriIndexesForMaterial(groupname))
end

timer('filtering faces', function()
	for i=#mesh.groups,1,-1 do
		local g = mesh.groups[i]
		if keepers[g.name] then
			print('keeping material '..g.name)
		else
			mesh.groups:remove(i)
		end
	end
end)

-- save will remove the rest
timer('saving', function()
	loader:save(outfn, mesh)
end)
