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
local plane3f = require 'vec-ffi.plane3f'
local quatf = require 'vec-ffi.quatf'
local json = require 'dkjson'
local vector = require 'ffi.cpp.vector'
local matrix_ffi = require 'matrix.ffi'
local Mesh = require 'mesh'
local OBJLoader = require 'mesh.objloader'

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

local function tileMesh(mesh, placeFn)
	if not mesh.triGroups then
		mesh:getTriPlanarGroups()
	end
	local triGroupForTri = mesh.triGroupForTri

	-- TOOD here with the group info

	-- why can't dkjson catch exceptions and insert line/col info? like my parser does.  grr..
	local placeInfo = assert(
		json.decode((assert(
			file(assert(
				placeFn,
				"failed to provide json filename"
			)):read(),
			"failed to read json file "..tostring(placeFn)
		))),
		"failed to decode json file "..tostring(placeFn)
	)

	local scale = vec3f(1,1,1)
	-- assert mesh ==  OBJLoader():load(assert(placeInfo.geometryFilename))

	-- TODO here maybe
	-- optimize omesh
	-- merge vtxs
	-- remove internal tris
	-- etc
	local omeshForFn = {}
	for _,inst in ipairs(placeInfo.surfaceInstances) do
		for _,geom in ipairs(inst.geometryArray) do
			if not omeshForFn[geom.filename] then
				local omesh = OBJLoader():load(assert(geom.filename))
				omesh:calcBBox()
				omesh:mergeMatchingVertexes()
				omeshForFn[geom.filename] = omesh
			end
		end
	end

	-- map y-up in brick to e_z = up on surface
	-- map z-length in brick to e_x = ∂/∂u on surface
	-- map x-thickness in brick to e_y = ∂/∂v on surface
	local spatialConvention = table{
		vec3f(0,1,0),
		vec3f(0,0,1),	-- model's y+ up maps to z+ which is then mapped to the tri normal dir
		vec3f(1,0,0),	-- model's z- fwd maps to x+ which is mapped to the tri ∂/∂u
	}


	mesh:generateTriBasis()

--[[ assert bcc works
-- TODO it's not cuz incredibly small tris are sneaking in there
	local bccEpsilon = 1e-6
	for ti=0,mesh.triIndexes.size-3,3 do
		local t = assert(mesh.tris[ti/3+1])
		local tp = mesh.triIndexes.v + ti
		local a,b,c = t:vtxPos(mesh)
		print('tri', ti/3, 'area', t.area)
		print(a,b,c)
		print(t:calcBCC(a, mesh))
		print(t:calcBCC(b, mesh))
		print(t:calcBCC(c, mesh))
		-- assert each point is at its respective basis element (1,0,0) (0,1,0) (0,0,1)
		assert((t:calcBCC(a, mesh) - vec3f(1,0,0)):norm() < bccEpsilon)
		assert((t:calcBCC(b, mesh) - vec3f(0,1,0)):norm() < bccEpsilon)
		assert((t:calcBCC(c, mesh) - vec3f(0,0,1)):norm() < bccEpsilon)
		-- assert that the COM is at (1/3, 1/3, 1/3)
		assert((t:calcBCC(t.com, mesh) - vec3f(1,1,1)/3):norm() < bccEpsilon)
	end
--]]

	local merged = Mesh()

	-- for each tri
	for _,surfInst in ipairs(placeInfo.surfaceInstances) do
		local offsetU, offsetV = table.unpack(surfInst.offsetUV)
		-- true = centered-rectangular lattice
		-- false = rectangular lattice
		local stack = surfInst.stacked
		-- how much to randomize placement
		-- TODO jitterOrientation
		local jitter = matrix_ffi(surfInst.jitterUV)
		-- TODO pick at random per location ... based on 'bias' sums
		local geomInst = surfInst.geometryArray[1]
		local omesh = assert(omeshForFn[geomInst.filename])

		-- list of column-vectors
		-- transform from uv-space to placement-space
		-- columns are [v-ofs | u-ofs] in the placement lattice
		-- hmm the config file says 'u offset' is the short offset and 'v offset' is long for bricks
		-- but right now i have 'u offset' go left and 'v offset' go down
		local placementCoordXForm = matrix_ffi{
			{offsetU, stack and 0 or offsetU/2},
			{0, offsetV},
		}

		local placementCoordXFormInv = placementCoordXForm:inv()
--print('placementXForm', placementCoordXForm)
--print('placementXFormInv', placementCoordXFormInv)
--assert((placementCoordXForm * placementCoordXFormInv - matrix_ffi{{1,0},{0,1}}):normSq() < 1e-7)



		mesh.tilePlaces = table()
		for ti=0,mesh.triIndexes.size-3,3 do
			local t = assert(mesh.tris[ti/3+1])
			local tp = mesh.triIndexes.v + ti
			local tvtxs = range(0,2):mapi(function(i)
				return mesh.vtxs.v[tp[i]]
			end)
			
print('placing tri '..t.index..' with group '..triGroupForTri[t].tris:mapi(function(t) return t.index end):concat', ')
print('...with '..#triGroupForTri[t].borderEdges..' clip planes '..triGroupForTri[t].borderEdges:mapi(function(info) return tostring(info.clipPlane) end):concat', ')
			local uvorigin2D = vec2f()
				-- uvorigin2D/uvorigin3D can be any texcoord/pos as long as they're from the same vtx
				+ tvtxs[1].texcoord
				-- add a small epsilon to make sure placement of the first meshes isn't right on a triangle edge, such that subsequent folds around edges might incur floating point error and cause a row of meshes to pass some epsilon and stop abruptly (as we saw happening on the target_complex curved wall model).
				+ vec2f(.01, .01)
			local uvorigin3D = vec3f():set(tvtxs[1].pos:unpack())
	--print('uv origin', ti, uvorigin2D, uvorigin3D)
		
			-- the transform from tile-mesh to surface scale/spatialConvention
			--  store for later
			local modelToSurfXForm = matrix3x3To4x4(t.basis)
				* matrix3x3To4x4(spatialConvention)
				* scaleMat4x4(scale)

			local omeshBBox = omesh.bbox

			-- [[ also store the bbox of the omesh under this transform?
			-- this might help some edges, but it causes overlaps on planar edges
			-- TODO these aren't in texcoord space, they're in the global mesh space ... 
			local cornersTC = range(0,7):mapi(function(corner)
				-- get omesh bbox corner
				local c = omeshBBox:corner(corner)
				-- convert to texcoord space
				local ctc = 
					matrix3x3To4x4(spatialConvention)
					* scaleMat4x4(scale)
					* matrix_ffi{c.s[0], c.s[1], c.s[2], 1}
				-- convert to texcoord space
				return matrix_ffi{ctc[1], ctc[2]}
			end)
			local cornersPlacement = cornersTC:mapi(function(ctc)
				-- convert to placement space
				return placementCoordXFormInv * ctc
			end)
			--print('tri has placement bbox\n', cornersPlacement:mapi(tostring):concat'\n\t')
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
			
				--[[ stretch in placement space to the placement coord
				placementBBox:stretch(vec2f(placementCoord:unpack()))
				--]]
				-- [[ don't just stretch the placement coord
				-- stretch the model's bbox in placement-space
				for _,cpl in ipairs(cornersPlacement) do
					placementBBox:stretch(vec2f((placementCoord + cpl):unpack()))
				end
				--]]
	--print('stretching', placementCoord)
			end
	--local from = box2f(placementBBox)
			placementBBox.min = placementBBox.min:map(math.floor) - 2
			placementBBox.max = placementBBox.max:map(math.ceil) + 2
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
					-- this is the lattice (unjittered) pos
					-- needs testing of position versus triangle b.c.c. to not cause gaps in the lattice
					local placePos = t.basis[1] * duv.x + t.basis[2] * duv.y + uvorigin3D
					-- add jitter later.  otherwise a lattice point could jitter outside of the triangle and fail the bcc test
					-- then you have a brick wall with a brick missing from the middle of it.
					local jitteredPos = placePos + t.basis[1] * jitterUV[1] + t.basis[2] * jitterUV[2]
					
					local xform = translateMat4x4(placePos) * modelToSurfXForm

					--[[ just place all
					allInside = true
					--]]
					-- [[
					-- test if it's if in tri (barycentric coord test) then continue
					-- use the unjittered positions for the test so we don't get holes in the lattice
					-- later we will bcc test the closest point on the placed mesh bbox to the tri
					local bcc = t:calcBCC(placePos, mesh)
					local posInside = bcc.x >= 0 and bcc.y >= 0 and bcc.z >= 0
					--]]
					-- [[ if any corners in placement-space are within the tri ... continue
					local allInside = true
					local anyInside = false
					for _,ctc in ipairs(cornersTC) do
						local cornerPos = t.basis[1] * ctc[1] + t.basis[2] * ctc[2] + placePos
						local cornerInside = t:insideBCC(cornerPos, mesh)
						allInside = allInside and cornerInside
						anyInside = anyInside or cornerInside
					end
					--]]
					-- [=[ if anywhere is touching the tri, then clip it ... by ... ???
					--if anyInside 
					--and not allInside 
					--then
					do
						
						--[[ 
						TODO SEPARATE THIS OUT AS AN ARBITRARY POLYHEDRA CLIPPING FUNCTION

						mesh inside poly algorithm ...
						for detecting inside a polygon via bsp
						es = all boundary edges
						while #es > 0 do
							e = pop an edge from es
							if our point is on the front of e's plane ...
								remove all edges whose segments are on the back side of e's plane 
								if there's no edges left then we are inside
							if our point is on the back of e's plane
								remove all edges whose segments are in front of e's plane 
								if there's no edges left, we're outside
						end
						
						but how about for clipping a mesh to arbitrary planes?
						for this I'll need clip() to return a mesh of what it cut away
						and then i'll have to do some merges ....
						--]]
						
						local anythingRemoved = false
						
						-- separate edgeInfos into a list of those with line segments touching the front of the plane
						-- and those with line segments touchign the back fo the plane
						-- a line segment can appear in both lists.
						local function getEdgesInFront(edgeInfos, plane)
							return edgeInfos:filter(function(info)
								local e = info.edge
								local v1 = e.planePos + e.plane.n * e.interval[1]
								local v2 = e.planePos + e.plane.n * e.interval[2]
								return plane:dist(v1) > 1e-4
									or plane:dist(v2) > 1e-4
							end)
						end
						local function clipEdgesAgainstEdges(edgeInfos, plane)
							return getEdgesInFront(edgeInfos, plane),
								getEdgesInFront(edgeInfos, -plane)
						end
						
						-- clip a mesh against a polygon
						local function clipMeshAgainstEdges(edgeInfos, clipped)
							local info = edgeInfos:remove()
							-- clone and clip against the -plane -> backMesh 
							local backMesh = clipped:clone()
							backMesh:clip(-info.clipPlane)
							if #backMesh.tris == 0 then
								backMesh = nil
							end
							-- clone and clip the plane -> frontMesh
							local frontMesh = clipped:clone()
							frontMesh:clip(info.clipPlane)
							if #frontMesh.tris == 0 then
								frontMesh = nil
							end
							-- if that plane was the list on our list ...
							if #edgeInfos == 0 then
								-- ... then toss the backMesh because it is outside
								-- and just return frontMesh (if we have it)
								backMesh = nil
							else
								local edgesFront, edgesBack = clipEdgesAgainstEdges(edgeInfos, info.clipPlane)
								if #edgesBack > 0 then
									if backMesh then
										backMesh = clipMeshAgainstEdges(edgesBack, backMesh)
									end
								else
									-- backMesh is fully outside the poly -- toss it
									backMesh = nil
								end
								if #edgesFront > 0 then
									if frontMesh then
										frontMesh = clipMeshAgainstEdges(edgesFront, frontMesh)
									end
								else
									-- frontMesh is now fully inside the poly, so keep it (if we have it)
								end
							end
							-- keep track of whether we lost any pieces
							if not frontMesh or not backMesh then
								anythingRemoved = true
							end
							-- return the combination of front and back meshes
							if frontMesh then
								if backMesh then
									frontMesh:combine(backMesh)
								end
								return frontMesh
							elseif backMesh then
								return backMesh
							end
						end
						local clipped = clipMeshAgainstEdges(
							table(triGroupForTri[t].borderEdges),
							omesh:clone():transform(xform)
						)
						-- if clipped is nil then everything was removed

						if not clipped then
							-- ... then the mesh is all outside
						elseif anythingRemoved then
							-- then part of the mesh was inside
							merged:combine(clipped)
						else
							-- all was inside
							allInside = true --posInside
						end
					end


	--print(uv, inside)
					if allInside then
					--testing:
					--if anyInside and not allInside then
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
						mesh.tilePlaces:insert{
							filename = geomInst.filename,
							xform = xform,
						}
					end
				end
			end
		end
print('#tilePlaces', #mesh.tilePlaces)
	end

	timer('merging placed meshes', function()
		-- place instances
		local nvtxs = vector'MeshVertex_t'
		local indexesPerGroup = {}
		for _,place in ipairs(mesh.tilePlaces) do
			local omesh = assert(omeshForFn[place.filename])
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
			indexesPerGroup[place.filename] = indexesPerGroup[place.filename] or table()
			for _,g in ipairs(omesh.groups) do
				indexesPerGroup[place.filename][g.name] = indexesPerGroup[place.filename][g.name] or table()
				for i=3*g.triFirstIndex,3*(g.triFirstIndex+g.triCount)-1 do
					local srci = omesh.triIndexes.v[i]
					assert(srci >= 0 and srci < omesh.vtxs.size)
					local dsti = srci + firstVtx
					assert(dsti >= firstVtx and dsti < lastVtx)
					indexesPerGroup[place.filename][g.name]:insert(dsti)
				end
			end
		end

		local ntris = vector'uint32_t'
		for fn, omesh in pairs(omeshForFn) do
			if indexesPerGroup[fn] then
				for _,g in ipairs(omesh.groups) do
					g.triFirstIndex = ntris.size
					for _,i in ipairs(indexesPerGroup[fn][g.name] or {}) do
						ntris:emplace_back()[0] = i
					end
					g.triCount = ntris.size - g.triFirstIndex
				end
			end
		end

		--assert(nvtxs.size == omesh.vtxs.size * #mesh.tilePlaces)
		--assert(ntris.size == omesh.triIndexes.size * #mesh.tilePlaces)
print('nvtxs.size', nvtxs.size)
print('ntris.size', ntris.size)

		file'placement.json':write(json.encode(
		{
			instances = mesh.tilePlaces:mapi(function(p)
				return {
					filename = assert(p.filename),
					-- row-major ... transpose?
					transform = range(16):mapi(function(i)
						return p.xform.ptr[i-1]
					end),
				}
			end),
		}, {indent=true}))

		-- replace
		mesh.vtxs = nvtxs
		mesh.triIndexes = ntris

		-- reset
		mesh:rebuildTris()
	end)

	if merged then
		mesh:combine(merged)
	end

	-- invalidate
	mesh.edges = nil
	mesh.edgeIndexBuf = nil
	mesh.edges2 = nil
	mesh.loadedGL = nil
	mesh.vtxBuf = nil
	mesh.vtxAttrs = nil
	mesh.vao = nil

	mesh:calcBBox()
	mesh:findEdges()
	mesh:calcCOMs()

	mesh.mtlFilenames = select(2, next(omeshForFn)).mtlFilenames
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
	local gl = require 'gl'
	gl.glPointSize(3)
	gl.glColor3f(1,1,0)
	gl.glBegin(gl.GL_POINTS)
	for _,p in ipairs(mesh.tilePlaces) do
		gl.glVertex3f(
			p.xform.ptr[3],
			p.xform.ptr[7],
			p.xform.ptr[11])
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

local function drawTileMeshPlanes(mesh)
	local gl = require 'gl'
	gl.glEnable(gl.GL_BLEND)
	gl.glDepthMask(gl.GL_FALSE)
	gl.glDisable(gl.GL_CULL_FACE)
	gl.glColor4f(1,1,0,.1)
	gl.glBegin(gl.GL_QUADS)
	for _,g in ipairs(mesh.triGroups) do
		for _,info in ipairs(g.borderEdges) do
			local e = info.edge
			local s0, s1 = table.unpack(e.interval)
			local v1 = e.planePos + e.plane.n * s0
			local v2 = e.planePos + e.plane.n * s1
			-- [[ make plane perpendicular to normal
			gl.glVertex3f((v1 + e.normAvg):unpack())
			gl.glVertex3f((v1 - e.normAvg):unpack())
			gl.glVertex3f((v2 - e.normAvg):unpack())
			gl.glVertex3f((v2 + e.normAvg):unpack())
			--]]
		end
	end
	gl.glEnd()
	gl.glEnable(gl.GL_CULL_FACE)
	gl.glDepthMask(gl.GL_TRUE)
	gl.glDisable(gl.GL_BLEND)

	-- now repeat and draw normals
	gl.glLineWidth(3)
	gl.glBegin(gl.GL_LINES)
	for _,g in ipairs(mesh.triGroups) do
		for _,info in ipairs(g.borderEdges) do
			local e = info.edge
			local s0, s1 = table.unpack(e.interval)
			local v1 = e.planePos + e.plane.n * s0
			local v2 = e.planePos + e.plane.n * s1
			local vavg = .5 * (v1 + v2)
			gl.glVertex3f((vavg + e.normAvg):unpack())
			gl.glVertex3f((vavg + e.normAvg + info.clipPlane.n * .5):unpack())
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

-- draw lines of triGroups[]  .borderEdges[]
-- this duplicates drawUnwrapUVEdges except for the mesh.triGroups
local function drawTileMeshEdges(mesh)
	if not mesh.triGroups then return end
	local alpha = .5
	local gl = require 'gl'
	gl.glLineWidth(3)
	gl.glEnable(gl.GL_BLEND)
	gl.glDepthMask(gl.GL_FALSE)
	gl.glBegin(gl.GL_LINES)
	for _,g in ipairs(mesh.triGroups) do
		for _,info in ipairs(g.borderEdges) do
			local e = info.edge
			local t1, t2 = table.unpack(e.tris)
			--local t2plane = t2.normal:cross(e.plane.n)
			--if t1.normal:dot(t2plane) > 0 then
			--	gl.glColor4f(0,1,0, alpha)
			--else
				gl.glColor4f(0,0,1, alpha)
			--end

			local s0, s1 = table.unpack(e.interval)
			local v1 = e.planePos + e.plane.n * s0
			local v2 = e.planePos + e.plane.n * s1
			v1 = v1 + e.normAvg * 1e-3
			v2 = v2 + e.normAvg * 1e-3
			gl.glVertex3fv(v1.s)
			gl.glVertex3fv(v2.s)
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)
	gl.glDepthMask(gl.GL_TRUE)
	gl.glDisable(gl.GL_BLEND)
end



return {
	tileMesh = tileMesh,
	drawTileMeshPlaces = drawTileMeshPlaces,
	drawTileMeshEdges = drawTileMeshEdges,
	drawTileMeshPlanes = drawTileMeshPlanes,
}
