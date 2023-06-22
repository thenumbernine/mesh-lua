--[[
mesh = mesh with texcoords
omesh = mesh to tile

mesh is modified
--]]
local ffi = require 'ffi'
local range = require 'ext.range'
local file = require 'ext.file'
local table = require 'ext.table'
local math = require 'ext.math'
local timer = require 'ext.timer'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local box2f = require 'vec-ffi.box2f'
local plane3f = require 'vec-ffi.plane3f'
local box3f = require 'vec-ffi.box3f'
local quatf = require 'vec-ffi.quatf'
local json = require 'dkjson'
local vector = require 'ffi.cpp.vector'
local matrix_ffi = require 'matrix.ffi'
local Mesh = require 'mesh'
local OBJLoader = require 'mesh.objloader'
local matrix3x3To4x4 = require 'mesh.common'.matrix3x3To4x4
local translateMat4x4 = require 'mesh.common'.translateMat4x4

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

local function scaleMat4x4(s)
	return matrix_ffi{
		{s.x,0,0,0},
		{0,s.y,0,0},
		{0,0,s.z,0},
		{0,0,0,1},
	}
end

--[[
TODO break this down into
1) find the tile placements / merge clipped meshes ... for surface
2) ... for edges
3) place tiles
--]]
local function tileMesh(mesh, placeFn)
	if not mesh.triGroups then
		mesh:calcTriSurfaceGroups()
	end
	if not mesh.edgeClipGroups then
		mesh:calcTriEdgeGroups()
	end
	local triGroupForTri = mesh.triGroupForTri
	-- this will calc edges2 also
	assert(mesh.edges2)

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

	-- assert mesh ==  OBJLoader():load(assert(placeInfo.geometryFilename))

	-- TODO here maybe
	-- optimize omesh
	-- merge vtxs
	-- remove internal tris
	-- etc
	local omeshForFn = {}
	local function loadWithBBox(fn)
		if not omeshForFn[fn] then
			local omesh = OBJLoader():load(assert(fn))
			-- tends to mess up the model, so don't do this:
			--omesh:mergeMatchingVertexes(true, true)
			-- we want at least the origin within the bbox.  otherwise we have problems of gaps in the roof.
			omesh:recenter(omesh:calcCOM2())
			omesh:calcBBox()
			omeshForFn[fn] = omesh
		end
	end
	for _,insts in ipairs{placeInfo.surfaceInstances, placeInfo.cornerInstances, placeInfo.edgeInstances} do
		for _,inst in ipairs(insts) do
			if inst.geometryFilename then	-- corner and edge
				loadWithBBox(inst.geometryFilename)
			elseif inst.geometryArray then	-- surf
				for _,geom in ipairs(inst.geometryArray) do
					loadWithBBox(geom.filename)
				end
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

	mesh.tilePlaces = table()

	local function mergeOrPlace(xform, fn, tg, extraClipPlanes)
		local omesh = omeshForFn[fn]

		local allInside

		if not tg then
			-- nothing to clip against.
			allInside = true
		else
			--[[ TODO prelim bbox test ...
			-- if any corners in placement-space are within the tri ... continue
			local allInside = true
			local anyInside = false
			for _,ctc in ipairs(cornersTC) do
				local cornerPos = pl.t.basis[1] * ctc[1] + pl.t.basis[2] * ctc[2] + pl.jitteredPos
				local cornerInside = pl.t:insideBCC(cornerPos, mesh)
				allInside = allInside and cornerInside
				anyInside = anyInside or cornerInside
			end
			--]]
			-- [=[ if anywhere is touching the tri, then clip it ... by ... ???
			--if anyInside
			--and not allInside
			--then
			local clipped, anythingRemoved = omesh:clone():transform(xform):clipToClipGroup(tg)
			-- I could introduce clip gruop manipulation
			-- but it is tied up in a BSP algorithm and edge line segment intervals for determining front/back of previously-clipped-planes so ...
			if clipped and extraClipPlanes then
				for _,extraClipPlane in ipairs(extraClipPlanes) do
					local anythingRemoved2 = clipped:clip(extraClipPlane)
					anythingRemoved = anythingRemoved or anythingRemoved2
				end
			end
			if not clipped then
				-- ... then the mesh is all outside
			elseif anythingRemoved then
				-- then part of the mesh was inside
				merged:combine(clipped)
			else
				allInside = true
			end
		end

		if allInside then
			-- all was inside
			mesh.tilePlaces:insert{
				filename = fn,
				xform = xform,
			}
		end
	end

	-- for each tri
	for _,surfInst in ipairs(placeInfo.surfaceInstances) do
		local offsetU, offsetV = table.unpack(surfInst.offsetUV)
		-- true = centered-rectangular lattice
		-- false = rectangular lattice
		local stack = surfInst.stacked
		-- how much to randomize placement
		-- TODO jitterOrientation
		local jitter = matrix_ffi(surfInst.jitterUV)

		local allPossibleSurfBBox = box3f.empty()
		for _,geomInst in ipairs(surfInst.geometryArray) do
			allPossibleSurfBBox:stretch(omeshForFn[geomInst.filename].bbox)
		end

		-- TODO pick at random based on 'bias' sums
		local geomInst = table.pickRandom(surfInst.geometryArray)
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

		for groupIndex,tg in ipairs(mesh.triGroups) do
			local placementsForThisGroup = {}
			for _,t in ipairs(tg.tris) do
				local ti = 3*(t.index-1)

				local tp = mesh.triIndexes.v + ti
				local tvtxs = range(0,2):mapi(function(i)
					return mesh.vtxs.v[tp[i]]
				end)

print('placing tri '..t.index..' with group of tris '..tg.tris:mapi(function(t) return t.index end):concat', ')
print('...with '..#tg.borderEdges..' clip planes '..tg.borderEdges:mapi(function(info) return tostring(info.clipPlane) end):concat', ')
				local uvorigin2D = vec2f()
					-- uvorigin2D/uvorigin3D can be any texcoord/pos as long as they're from the same vtx
					+ tvtxs[1].texcoord
					-- add a small epsilon to make sure placement of the first meshes isn't right on a triangle edge, such that subsequent folds around edges might incur floating point error and cause a row of meshes to pass some epsilon and stop abruptly (as we saw happening on the target_complex curved wall model).
					+ vec2f(.01, .01)
				local uvorigin3D = vec3f():set(tvtxs[1].pos:unpack())
--print('uv origin', ti, uvorigin2D, uvorigin3D)

				-- [[ also store the bbox of the omesh under this transform?
				-- this might help some edges, but it causes overlaps on planar edges
				-- TODO these aren't in texcoord space, they're in the global mesh space ...
				local cornersTC = range(0,7):mapi(function(corner)
					-- get omesh bbox corner
					local c = allPossibleSurfBBox:corner(corner)
					-- convert to texcoord space
					local ctc = matrix3x3To4x4(spatialConvention)
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
				local placementSize = placementBBox:size() + 1
				for pu=placementBBox.min.x,placementBBox.max.x+.01 do
					for pv=placementBBox.min.y,placementBBox.max.y+.01 do
-- if groups are contiguous unwrapped texcoords then there should be one (pu,pv) per group right?

						-- testing bbox for inside will cause double-occurrences in the lattice at edges on planar neighboring tris.  this is bad.
						-- but adding jitter before the test will cause some points to go outside and fail the test.  this is bad too.
						-- so I have to test without jitter, then later introduce jitter.
						local jitterUV = placementCoordXForm * matrix_ffi{
							pu + (math.random() * 2 - 1) * jitter[1],
							pv + (math.random() * 2 - 1) * jitter[2],
						}

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
						-- add jitter later.  otherwise a lattice point could jitter outside of the triangle and fail the bcc test
						-- then you have a brick wall with a brick missing from the middle of it.
						local jitteredPos = uvorigin3D
							+ t.basis[1] * (jitterUV[1] - uvorigin2D.x)
							+ t.basis[2] * (jitterUV[2] - uvorigin2D.y)

						-- test if it's if in tri (barycentric coord test) then continue
						-- use the unjittered positions for the test so we don't get holes in the lattice
						-- later we will bcc test the closest point on the placed mesh bbox to the tri
						local bcc = t:calcBCC(jitteredPos, mesh)
						local minbcc = math.min(bcc:unpack())

						local key = pu..','..pv
						local placement = placementsForThisGroup[key]
						if not placement then
							placementsForThisGroup[key] = {
								minbcc = minbcc,
								jitteredPos = jitteredPos,
								t = t,
							}
						elseif minbcc > placement.minbcc then	-- use the max of the minbcc
							placement.minbcc = minbcc
							placement.jitteredPos = jitteredPos
							placement.t = t
						end
					end
				end
			end

			local beforeTilePlaceCount = #mesh.tilePlaces
			for key,pl in pairs(placementsForThisGroup) do
				--local pu, pv = string.split(key,','):mapi(function(x) return tonumber(x) end):unpack()
				local xform = translateMat4x4(pl.jitteredPos)
					* matrix3x3To4x4(pl.t.basis)
					* matrix3x3To4x4(spatialConvention)
				mergeOrPlace(xform, geomInst.filename, tg)
			end
			print('group '..groupIndex..' placed '..(#mesh.tilePlaces - beforeTilePlaceCount)..' tiles')
		end
	end
print('#tilePlaces from surfaces', #mesh.tilePlaces)

-- [=[
	local numSurfTilePlaces = #mesh.tilePlaces
	local totalEdgesCovered = 0
	for _,eg in ipairs(mesh.edgeClipGroups) do
print("placing along edge group...")
print('...with '..#eg.borderEdges..' clip planes '..eg.borderEdges:mapi(function(info) return tostring(info.clipPlane) end):concat', ')
		totalEdgesCovered = totalEdgesCovered + #eg.srcEdges

		-- TODO pick every step?
		-- nah, 
		-- the edge group si already asserted to all have matching edge.isExtEdge
		-- but then how do we know how much to step if we haven't picked until after we step?
		-- is that what offsetDistance is supposed to be?
		-- yes?
		local insts
		if eg.srcEdges[1].edge.isExtEdge == nil then	-- edge ... only has 1 tri
			insts = placeInfo.edgeInstances
		else	-- corner
			insts = placeInfo.cornerInstances
			-- e.isExtEdge == false <=> concave
			-- e.isExtEdge == true <=> convex
		end

		-- sum up srcEdges[].edge arclength and then march along it, looking up edges as you go
		-- use srcEdge[].intervalIndex to tell which side of the edge is the start
		local edgeGroupLength = 0
print('edge group has '..#eg.srcEdges..' srcEdges')	
		for _,es in ipairs(eg.srcEdges) do
			local e = es.edge
			local s0, s1 = table.unpack(e.interval)
			assert(s0 <= s1)
			edgeGroupLength = edgeGroupLength + (s1 - s0)
		end
print('edge group has total arclength', edgeGroupLength)
		-- TODO is it all or is it pick-one?
		-- and if it's pick-one then how to work around multiple offsetDistance's per-instance?
		for _,inst in ipairs(insts) do
			local offsetWidth = inst.offsetWidth or 0
			local numInsts = edgeGroupLength / inst.offsetDistance
print('edge for inst', inst,'has',numInsts,'placements')
			local startNumTilesPlaced = #mesh.tilePlaces
			-- TODO how come i keep having to increase this ...
			local places = table()
			for i=-2,numInsts+2 do	-- plus one more for good measure,  i probalby have to clip this.
				local s = i * inst.offsetDistance
print('placing at arclength', s)				
				-- now find edge associated with this 's' ...
				local foundes 
				local foundj
				for j,es in ipairs(eg.srcEdges) do
					local e = es.edge
					local s0, s1 = table.unpack(e.interval)
					local slen = s1 - s0
					assert(slen >= 0)
					-- TODO here , pythagorean theorem / curve arclength
					-- if our next edge has turned a bit then we don't want to subtract off the full interval from our arclength parameter
					-- instead subtract off the arclength amount associated with the outer edge of this (based on some mesh or something)
					-- use inst.offsetWidth for this - which should be half the width of the mesh
					-- hmm but this now means we need a new arclength per instance ...				
					-- TODO first half of first edge line seg won't have angle influence ... next steps will
					if j < #eg.srcEdges then
						local theta = math.acos(math.clamp(math.abs(eg.srcEdges[j+1].edge.plane.n:dot(e.plane.n)), -1, 1))
						local ds = offsetWidth * math.tan(theta/2)
						--if j > 1 then ds = ds * 2 end
						-- inc the amount subtracted off of 's' arclength parameterization
						slen = slen + ds
					end
					if s <= slen 
					-- just pick the last if we haven't foudn one yet - for when we overflow the smax
					-- and do it before subtracting out the interval length
					or j == #eg.srcEdges 
					then
						foundes = es
						foundj = j
						break
					end
					s = s - slen
				end
				assert(foundes)
print('found arclength at '..foundj..'th edge with local arclength', s, 'and starting interval in edge group', foundes.intervalIndex)				
				-- except for the oob s range inst's we can also assert 0 <= sg <= (foundes.edge.interval difference)
				local e = foundes.edge
				local s0, s1 = table.unpack(e.interval)
				local savg = .5 * (s0 + s1)
				local v1, v2 = e:getPts()
				local edgeDir = e.plane.n
				if foundes.intervalIndex == 2 then
					-- then flip the interval and go from end to start ...
					v1, v2 = v2, v1
					-- don't flip both s and edgeDir or you'll double negative
					--s = savg - (s - savg)
					-- hmm why again do I not need to flip this when foundes.intervalIndex says we aren't starting at the first index?
					edgeDir = -edgeDir
				end
print('placing along edge from ',v1,'to',v2,'with pos', e.planePos, 'normal', e.plane.n)
			
				local omesh = omeshForFn[inst.geometryFilename]
				-- e.normAvg is the up axis, going to be y
				-- e.plane.n is the long axis, going to be z
				local ex = e.normAvg:cross(-e.plane.n)
				local ey = e.normAvg
				local ez = -e.plane.n
				--local pos = e.planePos + s * e.plane.n
				local pos = v1 + s * edgeDir
print('placing at interval param', s, 'pos', pos)
				-- store for now the pos and transform and clipgroup for this instance
				-- then later go between them and add an extra clip edge
				places:insert{
					pos = pos,
					basis = {ex,ey,ez},
					inst = inst,
					eg = eg,
					edge = e,
				}
			end
			-- now that we have all positions calculated and stored, insert new clipplanes between them
			for i,p in ipairs(places) do
				local extraCurveClipPlanes = table()
				if i > 1 then
					local pprev = places[i-1]
					-- failing for non-manifold meshes
					--assert(p.edge.plane.n:dot(pprev.edge.plane.n) > 0)
					if p.edge.plane.n:dot(pprev.edge.plane.n) <= 0 then
						print('!!! WARNING !!! group edge to next edge not aligned.  is the mesh non-manifold?')
					end
					local plane = plane3f():fromDirPt(
						(p.edge.plane.n + pprev.edge.plane.n):normalize(),
						(p.pos + pprev.pos) * .5)
					local tside = plane:test(p.pos)
					extraCurveClipPlanes:insert(tside and plane or -plane)
				end
				if i < #places then
					local pnext = places[i+1]
					-- fails on non-manifold meshes
					--assert(p.edge.plane.n:dot(pnext.edge.plane.n) > 0)
					if p.edge.plane.n:dot(pnext.edge.plane.n) <= 0 then
						print('!!! WARNING !!! group edge to next edge not aligned.  is the mesh non-manifold?')
					end
					local plane = plane3f():fromDirPt(
						(p.edge.plane.n + pnext.edge.plane.n):normalize(),
						(p.pos + pnext.pos) * .5)
					local tside = plane:test(p.pos)
					extraCurveClipPlanes:insert(tside and plane or -plane)
				end
				local xform = translateMat4x4(p.pos)
						* matrix3x3To4x4(p.basis)
				mergeOrPlace(xform, p.inst.geometryFilename, p.eg, extraCurveClipPlanes)
			end
			local numTilesPlacedForThisEdge = #mesh.tilePlaces - startNumTilesPlaced
print('placed', numTilesPlacedForThisEdge, 'unclipped tiles for this edge and inst')
		end
	end
print('# edges placed along', totalEdgesCovered)
print('#tilePlaces from edges', #mesh.tilePlaces - numSurfTilePlaces)
print('#tilePlaces total', #mesh.tilePlaces)
--]=]

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
					-- matrix_ffi is stored column-major
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
	mesh:unloadGL()

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
		-- matrix_ffi stores col-major
		gl.glVertex3f(
			p.xform.ptr[12],
			p.xform.ptr[13],
			p.xform.ptr[14])
	end
	gl.glEnd()
	gl.glPointSize(1)
	gl.glLineWidth(3)
	gl.glBegin(gl.GL_LINES)
	for _,p in ipairs(mesh.tilePlaces) do
		-- matrix_ffi stores col-major
		local ex = vec3f():map(function(x,i) return p.xform.ptr[i] end)
		local ey = vec3f():map(function(x,i) return p.xform.ptr[4+i] end)
		local ez = vec3f():map(function(x,i) return p.xform.ptr[8+i] end)
		local pos = vec3f():map(function(x,i) return p.xform.ptr[12+i] end)
		gl.glColor3f(1,0,0)
		gl.glVertex3f(pos:unpack())
		gl.glVertex3f((pos + .1 * ex):unpack())
		gl.glColor3f(0,1,0)
		gl.glVertex3f(pos:unpack())
		gl.glVertex3f((pos + .1 * ey):unpack())
		gl.glColor3f(0,0,1)
		gl.glVertex3f(pos:unpack())
		gl.glVertex3f((pos + .1 * ez):unpack())
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

return {
	tileMesh = tileMesh,
	drawTileMeshPlaces = drawTileMeshPlaces,
}
