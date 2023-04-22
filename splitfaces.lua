#!/usr/bin/env luajit
local WavefrontObj = require 'wavefrontobj.wavefrontobj'
local table = require 'ext.table'
local infn, outfn = ...
assert(infn and outfn, "expected "..arg[0].." input-filename output-filename")

local obj = WavefrontObj(infn)

--obj:splitFaces() ...
for mtl,fs in pairs(obj.fsForMtl) do
	for _,f in ipairs(fs.quads) do
		local a,b,c,d = table.unpack(f)
		fs.tris:append{{a,b,c},{a,c,d}}
	end
	fs.quads = table()
end

obj:save(outfn)
