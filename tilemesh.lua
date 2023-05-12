--[[
mesh = mesh with texcoords
omesh = mesh to tile

mesh is modified
--]]
local ffi = require 'ffi'
local range = require 'ext.range'
local file = require 'ext.file'
local table = require 'ext.table'
local timer = require 'ext.timer'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local box2f = require 'vec-ffi.box2f'
local quatf = require 'vec-ffi.quatf'
local json = require 'dkjson'
local vector = require 'ffi.cpp.vector'
local matrix_ffi = require 'matrix.ffi'

-- R is a table of column vec3f's, v is a vec3f
local function rotateVec(R, v)
	return R[1] * v.x + R[2] * v.y + R[3] * v.z
end

local function scaleVec(a, b)
	return vec3f(
		a.x * b.x,
		a.y * b.y,
		a.z * b.z)
end

-- a and b are tables of columns of vec3f's
-- TODO matrix_ffi
local function matrixMul3x3(a, b)
	-- c_ij = a_ik b_kj
	local c = range(3):mapi(function() return vec3f() end)
	for i=0,2 do
		for j=0,2 do
			local sum = 0
			for k=0,2 do
				sum = sum + a[k+1].s[i] * b[j+1].s[k]
			end
			c[j+1].s[i] = sum
		end
	end
	return c
end

local function translateMat4x4(t)
	return matrix_ffi{
		{1,0,0,t.x},
		{0,1,0,t.y},
		{0,0,1,t.z},
		{0,0,0,1},
	}
end

local function scaleMat4x4(s)
	return matrix_ffi{
		{s.x,0,0,0},
		{0,s.y,0,0},
		{0,0,s.z,0},
		{0,0,0,1},
	}
end

local function matrix3x3To4x4(b)
	local m = matrix_ffi{4,4}:zeros()
	for i=0,2 do
		for j=0,2 do
			m.ptr[i + 4 * j] = b[j+1].s[i]
		end
	end
	m.ptr[3 + 4 * 3] = 1
	return m
end

local function tileMesh(mesh, omesh)

	-- TODO HERE
	-- optimize omesh
	-- merge vtxs
	-- remove internal tris
	-- etc
	omesh = omesh:clone()
	omesh:calcBBox()
	omesh:mergeMatchingVertexes()

	-- list of column-vectors
	-- transform from uv-space to placement-space
--[[
	local scale = vec3f(.5, .5, .5)
	local placementCoordXForm = matrix_ffi{
		{1, 0},
		{0, 1},
	}
--]]
--[[
	local scale = vec3f(.1, .05, .1) * .7
	--local scale = vec3f(1,1,1)
	local placementCoordXForm = matrix_ffi{
		{.2, .1},
		{0, .1},
	}
	local jitter = {0,0}
	local spatialConvention = {vec3f(1,0,0),vec3f(0,1,0),vec3f(0,0,1)}
--]]

--[[ identity
	local spatialConvention = table{vec3f(1,0,0),vec3f(0,1,0),vec3f(0,0,1)}
--]]
-- [[ convert y-up models to z-up tangent-space triangle basis (x = ∂/∂u, y = ∂/∂v, z = normal)

	local scale = vec3f(1,1,1)
	-- bbox of brick:
	-- min: -0.054999999701977, -5.0268096352113e-09, -0.11500000208616
	-- max: 0.054999999701977, 0.07600000500679, 0.11500000208616
	-- size: 0.10999999940395, 0.076000012457371, 0.23000000417233
	-- size: 0.11, 0.076, 0.23
	-- size: [thickness, height, length]
	-- ... with an extra .01 gap ...
	local offsetU = .24
	local offsetV = .12

	-- true = centered-rectangular lattice
	-- false = rectangular lattice
	local stack = false

	-- columns are [v-ofs | u-ofs] in the placement lattice
	-- hmm the config file says 'u offset' is the short offset and 'v offset' is long for bricks
	-- but right now i have 'u offset' go left and 'v offset' go down
	local placementCoordXForm = matrix_ffi{
		{offsetU, stack and 0 or offsetU/2},
		{0, offsetV},
	}

	-- how much to randomize placement
	local jitter = matrix_ffi{.05, .05}

	-- TODO jitterOrientation

	-- map y-up in brick to e_z = up on surface
	-- map z-length in brick to e_x = ∂/∂u on surface
	-- map x-thickness in brick to e_y = ∂/∂v on surface
	local spatialConvention = table{
		vec3f(0,1,0),
		vec3f(0,0,1),	-- model's y+ up maps to z+ which is then mapped to the tri normal dir
		vec3f(1,0,0),	-- model's z- fwd maps to x+ which is mapped to the tri ∂/∂u
	}
--]]

	local placementCoordXFormInv = placementCoordXForm:inv()
--print('placementXForm', placementCoordXForm)
--print('placementXFormInv', placementCoordXFormInv)
--assert((placementCoordXForm * placementCoordXFormInv - matrix_ffi{{1,0},{0,1}}):normSq() < 1e-7)

	mesh:generateTriBasis()

	-- for each tri
	mesh.tilePlaces = table()
	for ti=0,mesh.triIndexes.size-3,3 do
		local t = assert(mesh.tris[ti/3+1])
		local tp = mesh.triIndexes.v + ti
		local tvtxs = range(0,2):mapi(function(i)
			return mesh.vtxs.v[tp[i]]
		end)
		-- uvorigin2D/uvorigin3D can be any texcoord/pos as long as they're from the same vtx
		local uvorigin2D = vec2f():set(tvtxs[1].texcoord:unpack())
		local uvorigin3D = vec3f():set(tvtxs[1].pos:unpack())
--print('uv origin', ti, uvorigin2D, uvorigin3D)
	
		-- the transform from tile-mesh to surface scale/spatialConvention
		--  store for later
		local modelToSurfXForm = matrix3x3To4x4(t.basis)
			* matrix3x3To4x4(spatialConvention)
			* scaleMat4x4(scale)

		--[[ also store the bbox of the omesh under this transform?
		-- this might help some edges, but it causes overlaps on planar edges
		local cornersTC = range(0,7):mapi(function(corner)
			-- get omesh bbox corner
			local cv = matrix_ffi{
				omesh.bbox.s[bit.band(corner,1)].s[0],
				omesh.bbox.s[bit.band(bit.rshift(corner,1),1)].s[1],
				omesh.bbox.s[bit.band(bit.rshift(corner,2),1)].s[2],
				1}
			-- convert to surface-space
			local ctc = modelToSurfXForm * cv
			return matrix_ffi{ctc[1], ctc[2]}
		end)
		local cornersPlacement = cornersTC:mapi(function(ctc)
			-- convert to placement space
			return placementCoordXFormInv * ctc
		end)
		--]]

		-- find uv min max
		-- maybe stretch bounds to include edges of placements?
		-- interpolate across uv
		-- find lattice locations where an instance should be placed
		-- place mesh
		local placementBBox = box2f.empty()
		for j=0,2 do
			assert(tp[j] >= 0 and tp[j] < mesh.vtxs.size)
			local v = mesh.vtxs.v[tp[j]]
			--local tc = v.texcoord
			-- for the sake of scale, we have to remap the texcoords using the orthornormalized basis (which was derived from the texcoords so e_x = ∂/∂u)
			-- this is already accomplished form unwrapUV(), but if the mesh hasn't been unwrapped this way then this will fix it.
			local dvpos = v.pos - uvorigin3D
			local tc = vec2f(
				dvpos:dot(t.basis[1]),
				dvpos:dot(t.basis[2])
			) + uvorigin2D
			local placementCoord = placementCoordXFormInv * matrix_ffi{tc.x, tc.y}
		
			-- [[ stretch in placement space to the placement coord
			placementBBox:stretch(vec2f(placementCoord:unpack()))
			--]]
			--[[ don't just stretch the placement coord
			-- stretch the model's bbox in placement-space
			for _,cpl in ipairs(cornersPlacement) do
				placementBBox:stretch(vec2f((placementCoord + cpl):unpack()))
			end
			--]]
--print('stretching', placementCoord)
		end
--local from = box2f(placementBBox)
		placementBBox.min = placementBBox.min:map(math.floor) - 1
		placementBBox.max = placementBBox.max:map(math.ceil) + 1
--print('placementBBox', from, 'to' , placementBBox)
		for pu=placementBBox.min.x,placementBBox.max.x+.01 do
			for pv=placementBBox.min.y,placementBBox.max.y+.01 do
				-- testing bbox for inside will cause double-occurrences in the lattice at edges on planar neighboring tris.  this is bad.
				-- but adding jitter before the test will cause some points to go outside and fail the test.  this is bad too.
				-- so I have to test without jitter, then later introduce jitter.
				local uv = placementCoordXForm * matrix_ffi{pu, pv}
				uv = vec2f(uv:unpack())
--print(uv)
				local duv = uv - uvorigin2D
				
				local jitterPlacement = matrix_ffi{
					(math.random() * 2 - 1) * jitter[1],
					(math.random() * 2 - 1) * jitter[2],
				}
				local jitterUV = placementCoordXForm * jitterPlacement

				--[[
				texcoord = uvbasis^-1 * (placePos - uvorigin3D) + uvorigin2D
				uvbasis * (texcoord - uvorigin2D) = placePos - uvorigin3D
				placePos = uvbasis * (texcoord - uvorigin2D) + uvorigin3D

				with placement-lattice transforms
				placementCoords = placementXForm * texcoord
				placementXFormInv * placementCoords = texcoord
				--]]
				local placePos = t.basis[1] * duv.x + t.basis[2] * duv.y + uvorigin3D
				-- [[
				-- if in tri (barycentric coord test) then continue
				-- TODO bcc test the closest point on the placed mesh bbox to the tri
				local inside = t:insideBCC(placePos, mesh)
				--]]
				--[[ if any corners in placement-space are within the tri ... continue
				local inside
				for _,ctc in ipairs(cornersTC) do
					local cornerPos = t.basis[1] * ctc[1] + t.basis[2] * ctc[2] + placePos
					if t:insideBCC(cornerPos, mesh) then
						inside = true
						break
					end
				end
				--]]

				-- add jitter later.  otherwise a lattice point could jitter outside of the triangle and fail the bcc test
				-- then you have a brick wall with a brick missing from the middle of it.
				placePos = placePos + t.basis[1] * jitterUV[1] + t.basis[2] * jitterUV[2]
--print(uv, inside)
				if inside then
					--[[
					then place an instance of omesh
					get the transform rotation and scale to the location on the poly
					if unwrapuv() was just run then .tri[] .uvbasis3D and 2D will still exist

					total transform of scale->rotate->place
					TODO store basis as a matrix_ffi ?
					TODO store a tris[] c buffer containing ... TBN frame, COM, area
					and for Lua stuff add a *new* table
					
					dstvtxpos = vertex location on tiled geometry
					srcvtxpos = vertex location in the source tile mesh
					dstvtxpos = placePos + uvbasis * spatialConvention * scale * srcvtxpos
					
					solve for srcvtxpos:
					srcvtxpos = scale^-1 * spatialConvention^-1 * uvbasis^-1 * (dstvtxpos - placePos)
					substitute srcvtxpos == bboxcorner:
					bboxcorner = scale^-1 * spatialConvention^-1 * uvbasis^-1 * (dstvtxpos - placePos)
				
					substitute placePos:
					placePos = uvbasis * (texcoord - uvorigin2D) + uvorigin3D
					dstvtxpos = uvbasis * (texcoord - uvorigin2D) + uvorigin3D + uvbasis * spatialConvention * scale * srcvtxpos
					dstvtxpos - uvorigin3D = uvbasis * (texcoord - uvorigin2D + spatialConvention * scale * srcvtxpos)
				
					substitute srcvtxpos == bboxcorner:
					dstvtxpos - uvorigin3D = uvbasis * (texcoord - uvorigin2D + spatialConvention * scale * bboxcorner)
					texcoord = uvbasis^-1 * (dstvtxpos - uvorigin3D) + uvorigin2D - spatialConvention * scale * bboxcorner
					--]]
					local xform = translateMat4x4(placePos) * modelToSurfXForm
					mesh.tilePlaces:insert{
						--[[ scale, rotate, offset ...
						pos = vec3f(placePos),	-- not so necessary
						basis = matrixMul3x3(t.basis, spatialConvention),
						scale = vec3f(scale),
						--]]
						-- transform
						xform = xform,
						-- not necessary
						uv = vec2f(uv),
					}
				end
			end
		end
	end
print('#tilePlaces', #mesh.tilePlaces)

	-- place instances
	local nvtxs = vector'MeshVertex_t'
	local indexesPerGroup = table()
	for _,place in ipairs(mesh.tilePlaces) do
		local firstVtx = nvtxs.size
		for i=0,omesh.vtxs.size-1 do
			local srcv = omesh.vtxs.v[i]
			local dstv = nvtxs:emplace_back()
			dstv.texcoord = srcv.texcoord
			-- scale, rotate, normalize the normals
			local n = srcv.normal
			local n4 = place.xform * matrix_ffi{n.x, n.y, n.z, 0}
			dstv.normal = vec3f(n4.ptr[0], n4.ptr[1], n4.ptr[2])
			-- scale, rotate, translate the positions
			-- TODO switch to y-up, because someone was a n00b when learning OpenGL a long time ago, and so now we all have to suffer.
			local p = srcv.pos
			local p4 = place.xform * matrix_ffi{p.x, p.y, p.z, 1}
			dstv.pos = vec3f(p4.ptr[0], p4.ptr[1], p4.ptr[2])
		end
		local lastVtx = nvtxs.size
		for _,g in ipairs(omesh.groups) do
			indexesPerGroup[g.name] = indexesPerGroup[g.name] or table()
			for i=3*g.triFirstIndex,3*(g.triFirstIndex+g.triCount)-1 do
				local srci = omesh.triIndexes.v[i]
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

	assert(nvtxs.size == omesh.vtxs.size * #mesh.tilePlaces)
	assert(ntris.size == omesh.triIndexes.size * #mesh.tilePlaces)
print('nvtxs.size', nvtxs.size)
print('ntris.size', ntris.size)

	file'placement.json':write(json.encode(mesh.tilePlaces:mapi(function(p)
		return {
			transform = range(16):mapi(function(i) return p.xform.ptr[i-1] end),
		}
	end), {indent=true}))

	-- replace
	mesh.vtxs = nvtxs
	mesh.triIndexes = ntris

	-- reset
	mesh:rebuildTris()

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
	g.triCount = mesh.triIndexes.size/3

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
		gl.glVertex3f((p.pos + p.basis[1]):unpack())
		gl.glColor3f(0,1,0)
		gl.glVertex3f(p.pos:unpack())
		gl.glVertex3f((p.pos + p.basis[2]):unpack())
		gl.glColor3f(0,0,1)
		gl.glVertex3f(p.pos:unpack())
		gl.glVertex3f((p.pos + p.basis[3]):unpack())
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

return {
	tileMesh = tileMesh,
	drawTileMeshPlaces = drawTileMeshPlaces,
}
