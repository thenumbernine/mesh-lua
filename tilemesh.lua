--[[
mesh = mesh with texcoords
omesh = mesh to tile

mesh is modified
--]]
local ffi = require 'ffi'
local range = require 'ext.range'
local table = require 'ext.table'
local vec2f = require 'vec-ffi.vec2f'
local box2f = require 'vec-ffi.box2f'
local quatf = require 'vec-ffi.quatf'
local vector = require 'ffi.cpp.vector'

local function tileMesh(mesh, omesh)
	local nvtxs = vector'MeshVertex_t'
	local ntris = vector'uint32_t'
-- why do all the reallocations cause a segfault?
nvtxs:reserve(5600)
ntris:reserve(10080)
	local numvtxs = 0
	local numtris = 0
	-- for each tri
	local places = table()
	for ti=0,mesh.triIndexBuf.size-3,3 do
		local t = assert(mesh.tris[ti/3+1])
		local tp = mesh.triIndexBuf.v + ti
		-- find uv min max
		-- maybe stretch bounds to include edges of placements?
		-- interpolate across uv
		-- find lattice locations where an instance should be placed
		-- place mesh
		local uvbb = box2f.empty()
		local tcs = {}
		local vs = {}
		for j=0,2 do
			assert(tp[j] >= 0 and tp[j] < mesh.vtxs.size)
			local v = mesh.vtxs.v[tp[j]]
			local tc = v.texcoord
			tcs[j+1] = tc
			vs[j+1] = v.pos
			uvbb:stretch(tc)
		end
--print('uvbb', uvbb)
		uvbb.min = uvbb.min:map(math.floor) - 1
		uvbb.max = uvbb.max:map(math.ceil) + 1
print('uvbb', uvbb)
		for u=uvbb.min.x,uvbb.max.x+.01,.2 do
			for v=uvbb.min.y,uvbb.max.y+.01,.2 do
--print(u,v)
				local uv = vec2f(u,v)
				local duv = uv - t.uvorigin2D
				-- uv = uvbasisT * (vtxpos - uvorigin3D) + uvorigin2D
				-- vtxpos = uvbasis * (uv - uvorigin2D) + uvorigin3D
				local vtxpos = t.uvbasisT[1] * duv.x + t.uvbasisT[2] * duv.y + t.uvorigin3D
				-- if in tri (barycentric coord test)

				local outside
				for j=1,3 do
					local dv = (vs[j%3+1] - vs[j]):cross(t.uvbasisT[3])
					if dv:dot(vtxpos - vs[j]) > 0 then
						outside = true
						break
					end
				end
				if not outside then
					-- then place an instance of omesh
					-- get the transform rotation and scale to the location on the poly
					-- if unwrapuv() was just run then .tri[] .uvbasis3D and 2D will still exist
					local angle = quatf():fromMatrix(t.uvbasisT)
						-- :conjugate()
					local firstVtx = nvtxs.size
					for i=0,omesh.vtxs.size-1 do
						local srcv = omesh.vtxs.v[i]
						local dstv = nvtxs:emplace_back()
						dstv.pos = srcv.pos
						dstv.texcoord = srcv.texcoord
						dstv.normal = srcv.normal
						-- TODO switch to y-up, because someone was a n00b when learning OpenGL a long time ago, and so now we all have to suffer.
						dstv.pos.x = dstv.pos.x * .05
						dstv.pos.y = dstv.pos.y * .05
						dstv.pos.z = dstv.pos.z * .05
						--dstv.pos = angle:rotate(dstv.pos)
						dstv.pos = dstv.pos + vtxpos
						numvtxs = numvtxs + 1
					end
					for i=0,omesh.triIndexBuf.size-1 do
						numtris = numtris + 1
						ntris:emplace_back()[0] = omesh.triIndexBuf.v[i] + firstVtx
					end
					places:insert{uv=uv, pos=vtxpos}
				end
			end
		end
	end
assert(nvtxs.size == omesh.vtxs.size * #places)
assert(ntris.size == omesh.triIndexBuf.size * #places)
print('#places', #places)
print('nvtxs.size', numvtxs)
print('ntris.size', numtris)
print('nvtxs.size', nvtxs.size)
print('ntris.size', ntris.size)
	mesh.vtxs = nvtxs
	mesh.triIndexBuf = ntris

	-- reset
	mesh.tris = range(mesh.triIndexBuf.size/3):mapi(function(i) return {index=i+1} end)

	-- invalidate
	mesh.edges = nil
	mesh.edgeIndexBuf = nil
	mesh.allOverlappingEdges = nil
	mesh.loadedGL = nil
	mesh.vtxBuf = nil
	mesh.vtxAttrs = nil
	mesh.vao = nil

print'tri vtxs'
for i=0,mesh.triIndexBuf.size-3,3 do
	print(mesh:triVtxPos(i))
end
	mesh:calcBBox()

	local g = mesh.groups[1]
	g.triFirstIndex = 0
	g.triIndexCount = mesh.triIndexBuf.size/3
end
return tileMesh
