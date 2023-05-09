--[[
mesh = mesh with texcoords
omesh = mesh to tile

mesh is modified
--]]
local ffi = require 'ffi'
local range = require 'ext.range'
local table = require 'ext.table'
local timer = require 'ext.timer'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local box2f = require 'vec-ffi.box2f'
local quatf = require 'vec-ffi.quatf'
local vector = require 'ffi.cpp.vector'
local matrix_ffi = require 'matrix.ffi'

local function inv2x2(m)
	local det = m[1][1] * m[2][2] - m[1][2] * m[2][1]
	return matrix_ffi{
		{m[2][2], -m[1][2]},
		{-m[2][1], m[1][1]}} * (1 / det)
end


-- generate 
local function clearTNB(mesh)
	for i,t in ipairs(mesh.tris) do
		t.uvbasisT = nil
	end
end

-- generate tangent, binormal, normal
local function generateTBN(mesh)
	--[[ make sure triangles have uvbasisT
	vectors are columns ...
	[T|B]' * (pos[i] - pos0) = tc[i] - tc0
	[T|B] = 3x2, ' is transpose is 2x3
	let dpos = pos[i] - pos0, so it is 3x3 with 1rd row 0
	... same with tc
	... and we can truncate those 0 rows
	[T|B] * [T|B]' * dpos = [T|B] * dtc
	[T|B] * [T|B]' = I 2x2 since T and B are orthogonal ... but not vice versa since [T|B] is 3x2
	dpos * dtc^-1 = [T|B] * dtc * dtc^-1
	[T|B] = dpos * dtc^-1
	--]]
	for i,t in ipairs(mesh.tris) do
		if not t.uvbasisT then
			local tp = mesh.triIndexBuf.v + 3*(i-1)
			local va = mesh.vtxs.v[tp[0]]
			local vb = mesh.vtxs.v[tp[1]]
			local vc = mesh.vtxs.v[tp[2]]
			local dpos1 = vb.pos - va.pos
			local dpos2 = vc.pos - va.pos
			local dtc1 = vb.texcoord - va.texcoord	-- only considering 2D of it
			local dtc2 = vc.texcoord - va.texcoord

			-- dtc is matrix with columns of dtc[i]
			local dtc = matrix_ffi{
				{dtc1:unpack()},
				{dtc2:unpack()},
			}:T()
			-- now 2x2 invert
			local dtcInv = inv2x2(dtc)
			-- get the cols
			local dtcInv1 = vec2f(dtcInv[1][1], dtcInv[2][1])
			local dtcInv2 = vec2f(dtcInv[1][2], dtcInv[2][2])

			local ex = vec3f(
				dtcInv1:dot(vec2f(dpos1.x, dpos2.x)),
				dtcInv1:dot(vec2f(dpos1.y, dpos2.y)),
				dtcInv1:dot(vec2f(dpos1.z, dpos2.z))
			):normalize()
			--[[ don't use ey ... just use N x ex...
			local ey = vec3f(
				dtcInv2:dot(vec2f(dpos1.x, dpos2.x)),
				dtcInv2:dot(vec2f(dpos1.y, dpos2.y)),
				dtcInv2:dot(vec2f(dpos1.z, dpos2.z))
			):normalize()
			--]]
			--[[ or use the delta as ex ...
			local ex = dpos1:normalize()
			--]]
			local n = dpos1:cross(dpos2):normalize()
			t.uvbasisT = {
				ex,
				n:cross(ex):normalize(),
				n,
			}
--print(i, table.unpack(t.uvbasisT), n:dot(ex), n:dot(ey))
		end
	end
end

local function tileMesh(mesh, omesh)

	local scale = vec3f(.1, .05, .1) * .9

	-- list of column-vectors
	-- transform from uv-space to placement-space
--[[	
	local uvxform = matrix_ffi{
		{1, 0},
		{0, 1},
	}
--]]	
-- [[	
	local uvxform = matrix_ffi{
		{.2, 0},
		{.1, .1},
	}:T()
--]]
	local uvxformInv = inv2x2(uvxform)
print('placement', uvxform)
print('placementInv', uvxformInv)

	generateTBN(mesh)

	-- for each tri
	self.tilePlaces = table()
	for ti=0,mesh.triIndexBuf.size-3,3 do
		local t = assert(mesh.tris[ti/3+1])
		local tp = mesh.triIndexBuf.v + ti
		local tvtxs = range(0,2):mapi(function(i)
			return mesh.vtxs.v[tp[i]]
		end)
		-- uvorigin2D/uvorigin3D can be any texcoord/pos as long as they're from the same vtx
		local uvorigin2D = tvtxs[1].texcoord
		local uvorigin3D = tvtxs[1].pos
		local tnormal = t.uvbasisT[3]
		-- find uv min max
		-- maybe stretch bounds to include edges of placements?
		-- interpolate across uv
		-- find lattice locations where an instance should be placed
		-- place mesh
		local placementBBox = box2f.empty()
		local vs = {}
		for j=0,2 do
			assert(tp[j] >= 0 and tp[j] < mesh.vtxs.size)
			local v = mesh.vtxs.v[tp[j]]
			vs[j+1] = v.pos
			local tc = uvxformInv * matrix_ffi{v.texcoord.x, v.texcoord.y}
			tc = vec2f(tc:unpack())
			placementBBox:stretch(tc)
--print('stretching', v.texcoord)
		end
		local from = box2f(placementBBox)
		placementBBox.min = placementBBox.min:map(math.floor) - 1
		placementBBox.max = placementBBox.max:map(math.ceil) + 1
--print('placementBBox', from, 'to' , placementBBox)
		for pu=placementBBox.min.x,placementBBox.max.x+.01 do
			for pv=placementBBox.min.y,placementBBox.max.y+.01 do
				local uv = uvxform * matrix_ffi{pu,pv}
				uv = vec2f(uv:unpack())
--print(uv)

				local duv = uv - uvorigin2D
				-- uv = uvbasisT * (vtxpos - uvorigin3D) + uvorigin2D
				-- vtxpos = uvbasis * (uv - uvorigin2D) + uvorigin3D
				local vtxpos = t.uvbasisT[1] * duv.x + t.uvbasisT[2] * duv.y + uvorigin3D
				-- if in tri (barycentric coord test)

				local outside
				for j=1,3 do
					local dv = (vs[j%3+1] - vs[j]):cross(tnormal)
					if dv:dot(vtxpos - vs[j]) > 0 then
						outside = true
						break
					end
				end
				if not outside then
					-- then place an instance of omesh
					-- get the transform rotation and scale to the location on the poly
					-- if unwrapuv() was just run then .tri[] .uvbasis3D and 2D will still exist
					self.tilePlaces:insert{
						-- scale, rotate, offset ...
						pos = vtxpos,	-- not so necessary
						uvbasisT = t.uvbasisT,
						scale = scale,
						-- not necessary
						uv = uv,
					}
				end
			end
		end
	end
print('#tilePlaces', #self.tilePlaces)
	
	-- place instances
	local nvtxs = vector'MeshVertex_t'
	local indexesPerGroup = table()
	for _,place in ipairs(self.tilePlaces) do
		local firstVtx = nvtxs.size
		for i=0,omesh.vtxs.size-1 do
			local srcv = omesh.vtxs.v[i]
			local dstv = nvtxs:emplace_back()
			dstv.pos = srcv.pos
			dstv.texcoord = srcv.texcoord
			-- scale, rotate, normalize the normals
			dstv.normal.x = srcv.normal.x * place.scale.x
			dstv.normal.y = srcv.normal.y * place.scale.y
			dstv.normal.z = srcv.normal.z * place.scale.z
			dstv.normal = place.uvbasisT[1] * dstv.normal.x
						+ place.uvbasisT[2] * dstv.normal.y
						+ place.uvbasisT[3] * dstv.normal.z
			dstv.normal = dstv.normal:normalize()
			-- scale, rotate, translate the positions
			-- TODO switch to y-up, because someone was a n00b when learning OpenGL a long time ago, and so now we all have to suffer.
			dstv.pos.x = dstv.pos.x * place.scale.x
			dstv.pos.y = dstv.pos.y * place.scale.y
			dstv.pos.z = dstv.pos.z * place.scale.z
			dstv.pos = place.uvbasisT[1] * dstv.pos.x
					+ place.uvbasisT[2] * dstv.pos.y
					+ place.uvbasisT[3] * dstv.pos.z
			dstv.pos = dstv.pos + place.pos
		end
		local lastVtx = nvtxs.size
		for _,g in ipairs(omesh.groups) do
			indexesPerGroup[g.name] = indexesPerGroup[g.name] or table()
			for i=3*g.triFirstIndex,3*(g.triFirstIndex+g.triCount)-1 do
				local srci = omesh.triIndexBuf.v[i]
				assert(srci >= 0 and srci < omesh.vtxs.size)
				local dsti = srci + firstVtx
				assert(dsti >= firstVtx and dsti < lastVtx)
				indexesPerGroup[g.name]:insert(dsti)
			end
		end
	end
	
	local ntris = vector'uint32_t'
	for _,g in ipairs(omesh.groups) do
		g.triFirstIndex = ntris.size
		for _,i in ipairs(indexesPerGroup[g.name] or {}) do
			ntris:emplace_back()[0] = i
		end
		g.triCount = ntris.size - g.triFirstIndex
	end

	assert(nvtxs.size == omesh.vtxs.size * #self.tilePlaces)
	assert(ntris.size == omesh.triIndexBuf.size * #self.tilePlaces)
print('nvtxs.size', nvtxs.size)
print('ntris.size', ntris.size)

	-- replace
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

	mesh:calcBBox()
	mesh:findEdges()
	mesh:calcCOMs()

	mesh.mtlFilenames = omesh.mtlFilenames
	local g = mesh.groups[1]
	g.triFirstIndex = 0
	g.triCount = mesh.triIndexBuf.size/3

	--[[ adds another 40 seconds for the cube->bricks simple example
	timer('merging after shellmapping', function()
		mesh:mergeMatchingVertexes()
	end)
	--]]
end

local function drawTileMeshPlaces(mesh)
	if not mesh.tilePlaces then return end
	gl.glPointSize(3)
	gl.glColor3f(1,1,0)
	gl.glBegin(gl.GL_POINTS)
	for _,p in ipairs(mesh.tilePlaces) do
		gl.glVertex3f(p.pos:unpack())
	end
	gl.glPointSize(1)
	gl.glLineWidth(3)
	gl.glBegin(gl.GL_LINES)
	for _,p in ipairs(mesh.tilePlaces) do
		gl.glColor3f(1,0,0)
		gl.glVertex3f(p.pos:unpack())
		gl.glVertex3f((p.pos + p.uvbasisT[1]):unpack())
		gl.glColor3f(0,1,0)
		gl.glVertex3f(p.pos:unpack())
		gl.glVertex3f((p.pos + p.uvbasisT[2]):unpack())
		gl.glColor3f(0,0,1)
		gl.glVertex3f(p.pos:unpack())
		gl.glVertex3f((p.pos + p.uvbasisT[3]):unpack())
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

return {
	tileMesh = tileMesh,
	drawTileMeshPlaces = drawTileMeshPlaces,
}
