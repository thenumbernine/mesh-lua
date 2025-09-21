#!/usr/bin/env luajit
local timer = require 'ext.timer'

local infn = ...
assert(infn , "expected "..arg[0].." input-filename")
local loader = require 'mesh.objloader'()

local mesh
timer('loading', function()
	mesh = loader:load(infn)
end)

-- [[ temp - output each group into a structure
local posBox = require 'vec-ffi.box3f'()
local texcoordBox = require 'vec-ffi.box3f'()
for i=0,mesh.vtxs.size-1 do
	posBox:stretch(mesh.vtxs.v[i].pos)
	texcoordBox:stretch(mesh.vtxs.v[i].texcoord)
end

print('pos box', posBox)
local posExtent = math.max(
	math.max(posBox.min:map(math.abs):unpack()),
	math.max(posBox.max:map(math.abs):unpack())
)
print('max abs pos', posExtent)

print('texcoord box', texcoordBox)
local texcoordExtent = math.max(
	math.max(texcoordBox.min:map(math.abs):unpack()),
	math.max(texcoordBox.max:map(math.abs):unpack())
)
print('max abs texcoord', texcoordExtent)


for _,g in ipairs(mesh.groups) do
	io.write(g.name,"='")
	for ti=g.triFirstIndex,g.triFirstIndex+g.triCount-1 do
		local tp = mesh.triIndexes.v + 3*ti
		for j=0,2 do
			local v = mesh.vtxs.v[tp[j]]
			--print('{'..pos
				--..', '..texcoord
			--	..', '..normal
			--	..'},')
			-- [[
			local function toshort(x) return bit.band(0xffff, math.floor(x)) end
			local pos = (v.pos / posExtent * 32767):map(toshort)
			--local texcoord = v.texcoord / texcoordExtent
			local normal = (v.normal * 32767):map(toshort)
			io.write(
				('%04x%04x%04x'):format(pos.x, pos.y, pos.z)
				--..('%04x%04x%04x'):format(normal.x, normal.y, normal.z)
			)
			--]]
			--[[
			local function tobyte(x) return bit.band(0xff, math.floor(x)) end
			local pos = (v.pos / posExtent * 127):map(tobyte)
			--local texcoord = v.texcoord / texcoordExtent
			local normal = (v.normal * 127):map(tobyte)
			print(
				('%02x%02x%02x'):format(pos.x, pos.y, pos.z)
				..('%02x%02x%02x'):format(normal.x, normal.y, normal.z)
			)
			--]]
		end
	end
	print("',")
end
--]]
