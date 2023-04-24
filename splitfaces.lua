#!/usr/bin/env luajit
local WavefrontObj = require 'wavefrontobj.wavefrontobj'
local table = require 'ext.table'
local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")

local obj = WavefrontObj(infn)

for mtlname in pairs(obj.mtllib) do
	local tris = table()
	for a,b,c in obj:triiter(mtlname) do
		tris:insert{a,b,c}
	end
	obj.fsForMtl[mtlname] = #tris > 0 and {[3] = tris} or nil
end

obj:save(outfn)
