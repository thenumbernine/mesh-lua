#!/usr/bin/env luajit
local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")

local loader = require 'wavefrontobj.objloader'()
local obj = loader:load(infn)

local table = require 'ext.table'
for mtlname, mtl in pairs(obj.mtllib) do
	local tris = table()
	for _,t in ipairs(mtl.triindexes) do
		tris:insert{table.unpack(t)}
	end
	obj.mtllib[mtlname].faces = #tris > 0 and {[3] = tris} or table()
end

loader:save(outfn, obj)
