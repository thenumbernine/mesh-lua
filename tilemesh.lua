--[[
mesh = mesh with texcoords
omesh = mesh to tile

mesh is modified
--]]
local ffi = require 'ffi'
local vec2f = require 'vec-ffi.vec2f'
local box2f = require 'vec-ffi.box2f'
local quatf = require 'vec-ffi.quatf'
local vector = require 'ffi.cpp.vector'

local function tileMesh(mesh, omesh)
	local nvtxs = vector'MeshVertex_t'
	local ntris = vector'uint32_t'
	-- for each tri
	for ti,t in ipairs(mesh.tris) do
		local i = 3*(ti-1)
		-- find uv min max
		-- maybe stretch bounds to include edges of placements?
		-- interpolate across uv
		-- find lattice locations where an instance should be placed 
		-- place mesh
		local uvbb = box2f.empty()
		for j=0,2 do
			local tc = mesh.vtxs.v[i+j].texcoord
			uvbb:stretch(vec2f(tc.x, tc.y))
		end
		uvbb.min = uvbb.min:map(math.floor) - 1
		uvbb.max = uvbb.max:map(math.ceil) + 1
		for u=uvbb.min.x,uvbb.max.x do
			for v=uvbb.min.y,uvbb.max.y do
				-- if in tri (barycentric coord test) then place an instance of omesh
				-- get the transform rotation and scale to the location on the poly
				-- if unwrapuv() was just run then .tri[] .uvbasis3D and 2D will still exist
				local pos = t.uvorigin3D + t.uvbasisT[1] * u + t.uvbasisT[2] * v
				local angle = quatf():fromMatrix(t.uvbasisT)
				local firstVtx = nvtxs.size
				for i=0,omesh.vtxs.size-1 do
					local srcv = omesh.vtxs.v[i]
					local dstv = ffi.new('MeshVertex_t', srcv)
					dstv.pos = angle:rotate(dstv.pos)
					nvtxs:push_back(dstv)
				end
				for i=0,omesh.triIndexBuf.size-1 do
					ntris:push_back(omesh.triIndexBuf.v[i] + firstVtx)
				end
			end
		end
	end
	mesh.vtxs = nvtxs
	mesh.triIndexBuf = ntris

	-- invalidate
	mesh.edges = nil
	mesh.edgeIndexBuf = nil
	mesh.bbox = nil
	mesh.tris = nil
	mesh.allOverlappingEdges = nil
	mesh.loadedGL = nil
	mesh.vtxBuf = nil
error'here'
end
return tileMesh
