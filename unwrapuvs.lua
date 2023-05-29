local table = require 'ext.table'
local math = require 'ext.math'
local range = require 'ext.range'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local quatf = require 'vec-ffi.quatf'

local function unwrapUVs(args)
	local mesh = assert(args.mesh)
	local angleThreshold = args.angleThreshold or 5
	local cosAngleThreshold = math.cos(math.rad(angleThreshold))

	mesh:breakAllVertexes()
	mesh:calcAllOverlappingEdges()
	-- invalidate
	mesh.vtxBuf = nil
	mesh.vtxAttrs = nil
	mesh.vao = nil

--[[ TODO put this all in its own function or its own app
	local numSharpEdges = 0
	for a,other in pairs(mesh.allOverlappingEdges) do
		for b,edge in pairs(other) do
			-- #tris == 0 is an edge construction error
			-- #tris == 1 is a sharp edge ... which means a non-convex
			-- #tris == 2 is good
			-- any more ... we have something weird
			if #edge.tris == 0 then
				error'here'
			elseif #edge.tris == 1 then
				numSharpEdges = numSharpEdges + 1
			elseif #edge.tris > 2 then
				print('found an edge with != 2 tris: ' ..#edge.tris)
			end
		end
	end
	print('numSharpEdges = '..numSharpEdges)

	-- how about count area per cube sides?
	-- total vector, l=0 s.h.
	local avgNormal = vec3f()
	for _,t in ipairs(mesh.tris) do
		avgNormal = avgNormal + t.normal * t.area
	end
	local avgNormalIsZero = avgNormal:normSq() < 1e-7
	if not avgNormalIsZero then avgNormal = avgNormal:normalize() end
	print('avg normal = '..avgNormal)

	-- the same idea as the l=1 spherical harmonics
	local areas = matrix{6}:zeros()
	for _,t in ipairs(mesh.tris) do
		local _,i = table.sup(t.normal:map(math.abs))
		assert(i)
		local dir = t.normal[i] > 0 and 1 or 2
		local index = dir + 2 * (i-1)
		areas[index] = areas[index] + t.area
	end
	print('per-side x plus/minus normal distribution = '..require 'ext.tolua'(areas))

	-- turned out to not be so helpful
	local bestNormal
-- TODO snap-to-axis for within epsilon
--	if not avgNormalIsZero then
--		bestNormal = matrix(avgNormal)
do--	else
		local _, besti = table.sup(areas)
		local bestdir = math.floor((besti-1)/2)+1
		bestNormal = matrix{0,0,0}
		bestNormal[bestdir] = bestdir%2 == 0 and -1 or 1
	end
	print('bestNormal', bestNormal)

	-- for all faces (not checked)
	--  traverse neighbors by edge and make sure the normals align
	--  complain if the normals flip
	--  or should this be robust enough to determine volume without correct normals / tri order?
	--  I'll assume ccw polys for now.
	local function findLocalIndex(t, v)
		for i=1,3 do
			if t[i].v == v then return i end
		end
	end
--]]

	local function getEdgeOppositeTri(e, t)
		--assert(#e.tris == 2)
		local t1,t2 = table.unpack(e.tris)
		if t2 == t then
			t1, t2 = t2, t1
		end
		assert(t1 == t)
		return t2, t1
	end
	-- returns true on success
	local function calcUVBasis(t, tsrc, esrc)
		assert(not t.uvs)
		-- t[1] is our origin
		-- t[1]->t[2] is our x axis with unit length
--print('t.index', t.index)
		local v = {mesh:triVtxPos(3 * (t.index - 1))}
--print('v', table.unpack(v))
		local d1 = v[2] - v[1]
		local d2 = v[3] - v[2]
		local n = d1:cross(d2)
		local nlen = n:norm()
--print('|d1 x d2| = '..nlen)
		if not math.isfinite(nlen)
		or nlen < 1e-9
		then
			--t.normal = d1:normalize()
			-- can't fold this because i'ts not a triangle ... it's a line
			-- should I even populate the uv fields?  nah, just toss it in the caller
			return
		end
		n = n / nlen
--print('n = '..n)
		local tnormal = vec3f(n:unpack())

		--if true then
		if not tsrc then	-- first basis
			t.uvorigin2D = vec2f(0,0)
			-- modularity for choosing which point on the tri is the uv origin
			--[[ use the first point
			t.uvorigin3D = vec3f(v[1]:unpack())
			--]]
			-- [[ use the y-lowest point
			-- good for roofs ... not for walls ...
			t.uvorigin3D = vec3f(v[
				select(2, range(3):mapi(function(i)
					return v[i].y
				end):inf())
			]:unpack())
--print('inserting unwrap origin at', mesh.tris:find(t)-1, t.uvorigin3D, t.com)
			local tcom = mesh.triCOM(mesh:triVtxPos(3*(t.index-1)))
			mesh.unwrapUVOrigins:insert(t.uvorigin3D * .7 + tcom * .3)
			--]]

--print('uv2D = '..t.uvorigin2D)
--print('uv3D = '..t.uvorigin3D)

			-- modularity for choosing initial basis
			--[[ use first base of the triangle
			local ex = d1:normalize()
			--]]
			--[[ preference to align the first axis in the xz plane
			-- first find the best option of the 3 deltas
			-- close to the same as choosing n cross y+
			-- but the first set of tris are not so good
			local d3 = v[1] - v[3]
			local ex
			if math.abs(d1.y) < math.abs(d2.y) then	-- d1 < d2
				if math.abs(d1.y) < math.abs(d3.y) then	-- d1 < d2 and d1 < d3
					ex = d1:normalize()
				else			-- d3 < d1 < d2
					ex = d3:normalize()
				end
			else	-- d2 < d1
				if math.abs(d2.y) < math.abs(d3.y) then	-- d2 < d1 and d2 < d3
					ex = d2:normalize()
				else			-- d3 < d2 < d1
					ex = d3:normalize()
				end
			end
			--]]
			--[[ instead of choosing lowest delta in xz plane ...
			-- first prioritize dy=0
			-- then prioritize dx=0 or dz = 0
			local d3 = v[1] - v[3]
			local ex = table{d1:normalize(),d2:normalize(),d3:normalize()}:sort(function(a,b)
				if not math.isfinite(a:normSq()) then return false end
				if not math.isfinite(b:normSq()) then return true end
				return range(3):mapi(function(i) return a[i] == 0 and 1 or 0 end):sum()
					> range(3):mapi(function(i) return b[i] == 0 and 1 or 0 end):sum()
			end)[1]:normalize()
			print('ex', ex)
			--]]
			-- [[ just use n cross y+
			-- BEST FOR CARTESIAN ALIGNED
			-- best for top
			local ex = n:cross(vec3f(0,1,0)):normalize()
			--]]
			--[[ use the most-signfiicantly-found axis-aligned normal:
			local ex = n:cross(bestNormal):normalize()
			--]]
			--[[ just use n cross x+ or z+ ...
			-- ... gets nans
			local ex = n:cross{0,0,1}:normalize()
			--]]
			--[[ pick whatever is most perpendicular to n and another cartesian basis
			-- a[i] = 1, i = sup(|n[i]|) gives same as n cross y+, good for tops, but crashes for sides.
			-- a[i] = 1, i = inf(|n[i]|) doesn't crash for sides but gives bad tops results.
			-- a[i+1] = 1, i = inf(|n[i]|) crashes on sides, but same as n cross y+ on top
			local _, i = table.sup(n:map(math.abs))
			local a = vec3f()
			a[i] = 1
			local ex = n:cross(a):normalize()
			assert(math.isfinite(ex:normSq()))
			--]]
			--[[ draw a line between the lowest two points
			if v[1].y > v[2].y then
				if v[1].y > v[3].y then	-- 1 highest
					ex = (v[3] - v[2]):normalize()
				else	-- 3 highest
					ex = (v[2] - v[1]):normalize()
				end
			else
				if v[2].y > v[3].y then	-- 2 highest
					ex = (v[1] - v[3]):normalize()
				else	-- 3 highest
					ex = (v[2] - v[1]):normalize()
				end
			end
			--]]
			--[[ most orthogonal to bestNormal take 2
			local d3 = v[1] - v[3]
			local ds = table{d1:normalize(), d2:normalize(), d3:normalize()}
			local dots = ds:mapi(function(d) return math.abs(d:dot(bestNormal)) end)
			local i = range(3):sort(function(a,b) return dots[a] < dots[b] end)[1]
			local ex = ds[i]
			ex = n:cross(ex):normalize()
			--]]
			--[[ always use the fallback
			local ex = vec3f()
			--]]

			-- fallback, if n is nan or zero
			-- n = x+ x- looks good, v us left/right
			-- n = z+ z- has v sideways
			local exNormSq = ex:normSq()
			if exNormSq < 1e-3						-- can't use zero
			or not math.isfinite(exNormSq)			-- can't use nan
			or math.abs(ex:dot(n)) > 1 - 1e-3	-- can't use ex perp to n
			then
--print('failed to find u vector based on bestNormal, picked ex='..ex)
				-- pick any basis perpendicular to 'n'
				local ns = range(3):mapi(function(i)
					local a = vec3f()
					a.s[i-1] = 1
					return n:cross(a)
				end)
--print('choices\n', ns)
				local lens = range(3):mapi(function(i) return ns[i]:normSq() end)
				local _, i = table.sup(lens)	-- best normal
--print('biggest cross '..i)
				ex = ns[i]:normalize()
--print('picking fallback ', ex)
--print('ex = '..ex)
				-- tangent space.  store as row vectors i.e. transpose, hence the T
				t.basis = table{
					n:cross(ex):normalize(),
					-ex,
					n,
				}
			else
--print('ex = '..ex)
				-- tangent space.  store as row vectors i.e. transpose, hence the T
				t.basis = table{
					ex,
					n:cross(ex):normalize(),
					n,
				}
			end

--print('ey = '..t.basis[2])
		else
			assert(tsrc.uvs)

--[[
tsrc.v3      tsrc.v2
	   *-------* t.v2
	   |   ___/|
	   |__/    |
tsrc.v1*-------*
	  t.v3   t.v1
--]]
--print('folding from', tsrc.index, 'to', t.index)
			--[[ using .edges
			local i11 = findLocalIndex(tsrc, esrc[1])	-- where in tsrc is the edge's first?
			local i12 = findLocalIndex(tsrc, esrc[2])	-- where in tsrc is the edge's second?
			local i21 = findLocalIndex(t, esrc[1])	-- where in t is the edge's first?
			local i22 = findLocalIndex(t, esrc[2])	-- where in t is the edge's second?
			assert(i11 and i12 and i21 and i22)
			assert(tsrc[i11].v == t[i21].v)	-- esrc[1] matches between tsrc and t
			assert(tsrc[i12].v == t[i22].v)	-- esrc[2] matches between tsrc and t
			assert(tsrc[i11].v == esrc[1])
			assert(tsrc[i12].v == esrc[2])
			assert(t[i21].v == esrc[1])
			assert(t[i22].v == esrc[2])
			--]]
			-- [[ using .allOverlappingEdges
			local i11 = esrc.triVtxIndexes[1]
			local i12 = i11 % 3 + 1
			local i21 = esrc.triVtxIndexes[2]
			local i22 = i21 % 3 + 1
			assert(i11 and i12 and i21 and i22)
			--]]
--print('edge local vtx indexes: tsrc', i11, i12, 't', i21, i22)
			-- tables are identical

			local isrc
			assert(tsrc.uvs and tsrc.uvs[i11])
			--if tsrc.uvs[i11] then
				isrc = i11
			--elseif tsrc.uvs[i12] then
			--	isrc = i12
			--else
			--	error("how can we fold a line when the src tri doesn't have uv coords for it?")
			--end
			t.uvorigin2D = vec2f(tsrc.uvs[isrc]:unpack())			-- copy matching uv from edge neighbor
			local tsrcp = mesh.triIndexes.v + 3 * (tsrc.index - 1)
			t.uvorigin3D = vec3f(mesh.vtxs.v[tsrcp[isrc-1]].pos:unpack())	-- copy matching 3D position
--print('uv2D = '..t.uvorigin2D)
--print('uv3D = '..t.uvorigin3D)

			-- modularity for choosing unwrap rotation
			--[[ reset basis every time. dumb.
			local ex = d1:normalize()
			t.basis = table{
				ex,
				n:cross(ex):normalize(),
				n,
			}
			--]]
			--[[ subsequent tri basis should be constructed from rotating the prev tri basis
			-- find the rotation from normal 1 to normal 2
			-- that'll just be the matrix formed from n1 and n2's basis ...
			local q = quatf():vectorRotate(tsrc.normal, t.normal)
			t.basis = table{
				q:rotate(tsrc.basis[1]),
				q:rotate(tsrc.basis[2]),
				n,
			}
			--]]
			-- [[ pick the rotation along the cardinal axis that has the greatest change
			-- BEST FOR CARTESIAN ALIGNED
			local tsrcnormal = tsrc.basis[3]
			local dn = tnormal - tsrcnormal
			local q
			if dn:normSq() < 1e-3 then
				q = quatf(0,0,0,1)
			else
				-- pick smallest changing axis in normal?
				local _, i = table.inf{dn:map(math.abs):unpack()}
				if i == 1 then
					local degrees = math.deg(math.atan2(n.z, n.y) - math.atan2(tsrcnormal.z, tsrcnormal.y))
--print(tnormal, tsrcnormal, dn, 'rot on x-axis by', degrees)
					q = quatf():fromAngleAxis(1, 0, 0, degrees)
				elseif i == 2 then
					local degrees = math.deg(math.atan2(n.x, n.z) - math.atan2(tsrcnormal.x, tsrcnormal.z))
--print(tnormal, tsrcnormal, dn, 'rot on y-axis by', degrees)
					q = quatf():fromAngleAxis(0, 1, 0, degrees)
				elseif i == 3 then
					local degrees = math.deg(math.atan2(n.y, n.x) - math.atan2(tsrcnormal.y, tsrcnormal.x))
--print(tnormal, tsrcnormal, dn, 'rot on z-axis by', degrees)
					q = quatf():fromAngleAxis(0, 0, 1, degrees)
				end
			end
--print('q', q)
--print('n', n)
--print('tsrc ex = '..tsrc.basis[1])
--print('tsrc ey = '..tsrc.basis[2])
			t.basis = table{
				q:rotate(tsrc.basis[1]),
				q:rotate(tsrc.basis[2]),
				n,
			}
			--]]

--print('|ez-n| = '..(q:rotate(tsrc.basis[3]) - n):norm())
--print('ex = '..t.basis[1])
--print('ey = '..t.basis[2])
		end

		t.uvs = t.uvs or {}
		for i=1,3 do
			local d = v[i] - t.uvorigin3D
--print('d = '..d)
			local m = {t.basis[1], t.basis[2]}
--print('m', table.unpack(m))
			-- wait if this is d:dot then ... this is really 'uvbasis' not transpose
			local md = vec2f(
				d:dot(t.basis[1]),
				d:dot(t.basis[2])
			)
--print('m * d = '..md)
			t.uvs[i] = md + t.uvorigin2D
--print('uv = '..t.uvs[i])
			if not math.isfinite(t.uvs[i]:normSq()) then
				print('tri has nans in its basis')
			end
		end
		return true
	end

	-- debug information
	-- keep track of how it's made for visualization's sake ...
	mesh.unwrapUVOrigins = table()
	mesh.unwrapUVEdges = table()

	local notDoneYet = table(mesh.tris)
	local done = table()

	-- roofs
	local function calcUVBasisAndAddNeighbors(t, tsrc, e, todo)
		if tsrc then
--print('unwrapping across tris', mesh.tris:find(t)-1, mesh.tris:find(tsrc)-1, 'edge', mesh.allOverlappingEdges:find(e)-1)
			mesh.unwrapUVEdges:insert{tsrc, t, e}
		end
		-- calc the basis by rotating it around the edge
		assert((tsrc == nil) == (e == nil))
		-- roof actually looks good with always retarting ... but not best
		if calcUVBasis(t, tsrc, e) then
			done:insert(t)
			assert(t.uvs)
			-- insert neighbors into a to-be-calcd list
--print('tri', t.index)
			for _,e in ipairs(t.allOverlappingEdges) do
--print('edge length', e.length)
				-- for all edges in the t, go to the other faces matching.
				-- well, if there's more than 2 faces shared by an edge, that's a first hint something's wrong.
				do--if #e.tris == 2 then	-- if we're using any overlapping edge then this guarantee goes out the window
					local t2 = getEdgeOppositeTri(e, t)
-- if our tri
-- ... isn't in the 'todo' pile either ...
-- ... is still in the notDoneYet pile ...
					if not todo:find(t2)
					and not done:find(t2)
					then
						local i = notDoneYet:find(t2)
						if i then
							assert(not t2.uvs)
							notDoneYet:remove(i)
							todo:insert(t2)
						end
					end
				end
			end
		end
	end

	-- walls
	local function floodFillMatchingNormalNeighbors(t, tsrc, e, alreadyFilled)
		alreadyFilled:insertUnique(t)
		if t.uvs then return end
		if tsrc then
--print('flood-fill unwrapping across tris', mesh.tris:find(t)-1, mesh.tris:find(tsrc)-1, 'edge', mesh.allOverlappingEdges:find(e)-1)
			mesh.unwrapUVEdges:insert{tsrc, t, e, floodFill=true}
		end
		assert((tsrc == nil) == (e == nil))
		if calcUVBasis(t, tsrc, e) then
			done:insert(t)
			assert(t.uvs)
			for _,e in ipairs(t.allOverlappingEdges) do
				do--if #e.tris == 2 then
					local t2 = getEdgeOppositeTri(e, t)
					if not alreadyFilled:find(t2) then
						local tnormal = mesh.triNormal(mesh:triVtxPos(3*(t.index-1)))
						local t2normal = mesh.triNormal(mesh:triVtxPos(3*(t2.index-1)))
--print('flood fill testing', math.deg(math.acos(math.clamp(tnormal:dot(t2normal), -1, 1))))
						if tnormal:dot(t2normal) > cosAngleThreshold then
							floodFillMatchingNormalNeighbors(t2, t, e, alreadyFilled)
						else
							alreadyFilled:insertUnique(t2)
						end
					end
				end
			end
		end
	end

	while #notDoneYet > 0 do
--print('starting unwrapping process with '..#notDoneYet..' left')

		-- I will be tracking all live edges
		-- so process the first tri as the starting point
		-- then add its edges into the 'todo' list

		-- modularity heuristic of picking best starting edge
		--[[ take the first one regardless
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[[ largest tri first
		notDoneYet:sort(function(a,b) return a.area > b.area end)
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[[ choose the first edge that starts closest to the ground (lowest y value)
		-- ... but this does some whose sharp edges touches
		-- i really want only those with flat edges at the base
		notDoneYet:sort(function(a,b)
			return math.min(
				mesh.vtxs.v[a[1].v-1].pos.y,
				mesh.vtxs.v[a[2].v-1].pos.y,
				mesh.vtxs.v[a[3].v-1].pos.y
			) < math.min(
				mesh.vtxs.v[b[1].v-1].pos.y,
				mesh.vtxs.v[b[2].v-1].pos.y,
				mesh.vtxs.v[b[3].v-1].pos.y
			)
		end)
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[[ same as above but pick the lowest *edge* , not *vtx*, cuz we want the base edges aligned with the bottom
		notDoneYet:sort(function(a,b)
			local aEdgeYMin = range(3):mapi(function(i)
				return .5 * (mesh.vtxs.v[a[i].v-1].pos.y + mesh.vtxs.v[a[i%3+1].v-1].pos.y)
			end):inf()
			local bEdgeYMin = range(3):mapi(function(i)
				return .5 * (mesh.vtxs.v[b[i].v-1].pos.y + mesh.vtxs.v[b[i%3+1].v-1].pos.y)
			end):inf()
			return aEdgeYMin < bEdgeYMin
		end)
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[=[ choose *all* tris whose flat edges are at the minimum y
		local vtxsNotDoneYet = {}
		for _,t in ipairs(notDoneYet) do
			for i=1,3 do
				vtxsNotDoneYet[t[i].v] = true
			end
		end
		-- convert set of keys to list
		vtxsNotDoneYet = table.keys(vtxsNotDoneYet):sort(function(a,b)
			return mesh.vtxs.v[a-1].pos.y < mesh.vtxs.v[b-1].pos.y	-- sort by y axis
		end)
		local eps = (mesh.bbox.max[2] - mesh.bbox.min[2]) * 1e-5
		local ymin = mesh.vtxs.v[vtxsNotDoneYet[1]-1].pos.y
		print('y min', ymin)
		-- now go thru all tris not done yet
		-- if any have 2/3 vertexes at the min then add them
		local todo = table()
		for i=#notDoneYet,1,-1 do
			local t = notDoneYet[i]
			local mincount = 0
			for j=1,3 do
				if mesh.vtxs.v[t[j].v-1].pos.y < ymin + eps then mincount = mincount + 1 end
			end
			if mincount >= 2 then
				todo:insert(notDoneYet:remove(i))
			end
		end
		-- if none were added then add one with 1/3 vertexes at the min
		if #todo == 0 then
			for i=#notDoneYet,1,-1 do
				local t = notDoneYet[i]
				for j=1,3 do
					if mesh.vtxs.v[t[j].v-1].pos.y < ymin + eps then
						todo:insert(notDoneYet:remove(i))
						break
					end
				end
				if #todo > 0 then break end
			end
		end
--print('number to initialize with', #todo)
		-- ... and process them all once first, adding their neigbors to the 'todo' pile
		--]=]
		-- [=[ choose tris with any edges that are 90' from the guide normal
		-- but not if the vector from the com to the edges is towards the guide normal
		local todo = table()
		for i=#notDoneYet,1,-1 do
			local t = notDoneYet[i]
			local tp = mesh.triIndexes.v + 3 * (t.index - 1)
			local tcom = mesh.triCOM(mesh:triVtxPos(3*(i-1)))
			for j=1,3 do
				local a = mesh.vtxs.v[tp[j-1]].pos
				local b = mesh.vtxs.v[tp[j%3]].pos
				local edgeDir = b - a
				-- [[ if the edges is in the xz plane ...
				-- needed for basic walls to look good
				if math.abs(edgeDir:normalize():dot(vec3f(0,1,0))) < 1e-5 then
				--]] do
					local edgeCenter = (b + a) * .5
					local comToEdge = edgeCenter - tcom
--print('comToEdge', comToEdge)
					-- [[ exclude tops.  necessary for roofs.  helps walls too.
					--if comToEdge:dot(bestNormal) > 0 then
					if comToEdge:dot(vec3f(0,1,0)) > 0 then
					--]] do
						--[[ exclude edges pointed straight downward
						if math.abs(tnormal:cross(edgeDir):dot(vec3f(0,1,0))) > 1 - 1e-3 then
						--]] do
							--[[ exclude normals that aren't 90' in the xz plane ...
							-- necessary for rounded walls
							if math.abs(tnormal[1]) > 1 - 1e-3
							or math.abs(tnormal[3]) > 1 - 1e-3
							then
							--]] do
								notDoneYet:remove(i)
								todo:insert(t)
								break
							end
						end
					end
				end
			end
		end
		-- if finding a y-perpendicular downward-pointing edge was too much to ask,
		-- ... then pick one at random?
		-- this is used when seeding the roof
		if #todo == 0 then
--print("couldn't find any perp-to-bestNormal edges to initialize with... picking one at random")
			todo:insert(notDoneYet:remove(1))
		else
--print("found "..#todo.." perp-to-bestNormal edges to initialize with")
		end
		--]=]
		-- [[ sort 'todo'
		todo:sort(function(ta,tb)
			local a1, a2, a3 = mesh:triVtxPos(3*(ta.index-1))
			local minax = math.min(a1.x, a2.x, a3.x)
			local minay = math.min(a1.y, a2.y, a3.y)
			local minaz = math.min(a1.z, a2.z, a3.z)
			local b1, b2, b3 = mesh:triVtxPos(3*(tb.index-1))
			local minbx = math.min(b1.x, b2.x, b3.x)
			local minby = math.min(b1.y, b2.y, b3.y)
			local minbz = math.min(b1.z, b2.z, b3.z)
			-- sort by lowest y first
			-- this is what makes v-texcoord align with the bottom of the roof
			-- but for xz-plane rounded walls we don't want ymin to be a priority ... instead we want the xz to be a priority ...
			-- so only sort like this if the normal is pointing in a cartesian direction?
			-- nah that seems too complicated -- instead try to just filter out these rounded walls from the 'todo' seed in the first place.
			if minay < minby then return true end
			if minay > minby then return false end
			-- [[
			-- sort by x and z next
			-- this is what makes the u-texcoord align with the furthest sides of the  90' aligned walls
			-- sort by x
			if minax < minbx then return true end
			if minax > minbx then return false end
			-- sort by z
			if minaz < minbz then return true end
			if minaz > minbz then return false end
			--]]
			return minaz < minbz
			-- sort by area
			--return a.area > b.area
		end)
		todo = todo:reverse()
		--]]

		-- [=[ first pass to make sure all the first picked are considered
		-- during this first pass, immediately fold across any identical normals
		-- this is required for v=0 to align with roof bottom edges
--print('starting first pass with #todo', #todo)
		for i=#todo,1,-1 do
			local t = todo:remove(i)
			-- for 't', flood-fill through anything with matching normal
			-- while flood-filling, continue adding neighbors to 'todo'
			local filled = table()		-- allow overwriting of other seeded locations
			--local filled = table(todo)	-- don't
			floodFillMatchingNormalNeighbors(t, nil, nil, filled)
			--[[
			-- helpes for curved walls
			-- MUST NOT be used for the roof with multiple trim levels
			for _,t in ipairs(filled) do
				if not t.uvs then
					todo:insertUnique(t)
				end
			end
			--]]
		end
--print('after first pass, #todo', #todo, '#done', #done)
		--]=]

		while #todo > 0 do
			local t, tsrc, e

			-- pick best edge between any triangle in 'done' and any in 'todo'
			local edgesToCheck = table()
			for _,t in ipairs(todo) do
				for _,e in ipairs(t.allOverlappingEdges) do
					do--if #e.tris == 2 then
						local t2 = getEdgeOppositeTri(e, t)
						if done:find(t2) then
							edgesToCheck:insert{tri=t, edge=e, prevtri=t2}
						end
					end
				end
			end
			if #edgesToCheck == 0 then
				-- same as first iter
				print("no edges to check ...")
				t = todo:remove(math.random(#todo))
			else
				-- assert from prevoius iteration that the first is the best
				-- modularity heuristic for picking best continuing edge
				-- sort last instead of first, so first iteration and first entry is removed, so I can guarantee that all entries have .prevtri and .edge
				edgesToCheck:sort(function(a,b)
					local ea, eb = a.edge, b.edge
					--[[ prioritize longest edge ... cube makes a long straight shape with one bend.
					-- looks best for cone.  just does two solid pieces for the base and sides
					-- looks best for cube.  does the cubemap t.
					return ea.length > eb.length
					--]]
					--[[ prioritize shortest edge ... cube makes a zigzag
					return ea.length < eb.length
					--]]
					--[[ prioritize biggest area
					local atriarea = a.tri.area + a.prevtri.area
					local btriarea  = b.tri.area + b.prevtri.area
					return atriarea > btriarea
					--]]
					--[[ prioritize smallest area
					local atriarea = a.tri.area + a.prevtri.area
					local btriarea  = b.tri.area + b.prevtri.area
					return atriarea < btriarea
					--]]
					-- [[ prioritize rotation angle
					-- LOOKS THE BEST SO FAR
					local ra = atri.normal:dot(a.prevtri.normal)
					local rb = btri.normal:dot(b.prevtri.normal)
					return ra > rb
					--]]
					-- TODO Try prioritizing discrete curvature (mesh angle & area info combined?)
					--[[ prioritize by rotation.
					-- first priority is no-rotation
					-- next is predominantly y-axis rotations
					local dn = a.tri.normal - a.prevtri.normal
					local _, i = table.inf(dn:map(math.abs))
					--]]
				end)
				local check = edgesToCheck[1]
				t, e, tsrc = check.tri, check.edge, check.prevtri
				assert(t)
				assert(e)
				assert(tsrc)
				assert(tsrc.uvs)
				todo:removeObject(t)
			end
			for _,t in ipairs(todo) do
				assert(not t.uvs)
			end
			if t then
				calcUVBasisAndAddNeighbors(t, tsrc, e, todo)
			end
		end
		for _,t in ipairs(done) do
			assert(t.uvs)
		end
	end

	-- replace?
	-- first explode
	mesh.vts = table()
	for i=1,#mesh.tris do
		local t = mesh.tris[i]
		for j=1,3 do
			local src = t.uvs and t.uvs[j] or vec2f(0,0)
			mesh.vtxs.v[(j-1)+3*(i-1)].texcoord:set(src.x, src.y, 0)
		end
		t.uvs = nil
	end

	print('tri basis:')
	for i,t in ipairs(mesh.tris) do
		if t.basis then
			print(i, t.basis:unpack())
		end
	end

	print('flood-fill-normals touched '..#mesh.unwrapUVEdges:filter(function(u) return u.floodFill end))
end

function drawUnwrapUVGraph(mesh)
	local gl = require 'gl'
	local eps = 1e-3
	-- [[ show unwrap info
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for _,info in ipairs(mesh.unwrapUVEdges or {}) do
		local ta, tb, e = table.unpack(info)
		if info.floodFill then
			gl.glColor3f(0,0,1)
		else
			gl.glColor3f(0,1,0)
		end

		local taa, tab, tac = mesh:triVtxPos(3*(ta.index-1))
		local tacom = (taa + tab + tac) * (1/3)
		local tanormal = mesh.triNormal(taa, tab, tac)

		local tba, tbb, tbc = mesh:triVtxPos(3*(tb.index-1))
		local tbcom = (tba + tbb + tbc) * (1/3)
		local tbnormal = mesh.triNormal(tba, tbb, tbc)

		gl.glVertex3f((tacom + tanormal * eps):unpack())

		-- [=[
		if not info.floodFill then
			gl.glColor3f(.5,.5,0)
		end
		local t1 = e.tris[1]
		local tp1 = mesh.triIndexes.v + 3 * (t1.index - 1)
		local t2 = e.tris[2]
		local tp2 = mesh.triIndexes.v + 3 * (t2.index - 1)
		assert((t1 == ta and t2 == tb) or (t1 == tb and t2 == ta))
		local vi11 = tp1[e.triVtxIndexes[1]-1]
		local vi12 = tp1[e.triVtxIndexes[1]%3]
		local vi21 = tp2[e.triVtxIndexes[2]-1]
		local vi22 = tp2[e.triVtxIndexes[2]%3]
		local v11 = mesh.vtxs.v[vi11].pos	-- this's intervals is along the opposing tri's edge
		local v12 = mesh.vtxs.v[vi12].pos
		local v21 = mesh.vtxs.v[vi21].pos	-- e.intervals[2][1] is always 0
		local v22 = mesh.vtxs.v[vi22].pos	-- e.intervals[2][2] is always its edge length
		local l = e.intervals[2][2]
		-- pick the subset of intervals in common
		local lmin = math.max(e.intervals[1][1], e.intervals[2][1])
		local lmax = math.min(e.intervals[1][2], e.intervals[2][2])
		-- pick its average
		local s = .5 * (lmin + lmax)
		-- find its ratio along the fixed interval of the 2nd tri's edge
		local t = s / l
		local edgeCom = ((v21 * (1 - t) + v22 * t) + (tanormal + tbnormal):normalize() * eps)
		gl.glVertex3f(edgeCom:unpack())
		gl.glVertex3f(edgeCom:unpack())
		--]=]

		if not info.floodFill then
			gl.glColor3f(1,0,0)
		end
		gl.glVertex3f((tbcom + tbnormal * eps):unpack())
	end
	gl.glEnd()
	gl.glPointSize(3)
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_POINTS)
	for _,v in ipairs(mesh.unwrapUVOrigins or {}) do
		gl.glVertex3f(v:unpack())
	end
	gl.glEnd()
	gl.glPointSize(1)
	--]]
end

-- draw the edges that are folded over.
function drawUnwrapUVEdges(mesh, angleThreshold)
	angleThreshold = angleThreshold or 5
	local cosAngleThreshold = math.cos(math.rad(angleThreshold))
	
	local gl = require 'gl'
	gl.glLineWidth(3)
	gl.glBegin(gl.GL_LINES)
	for _,e in ipairs(mesh.allOverlappingEdges or {}) do
		if e then
			-- out of all edges, find edges that *aren't* planar ...
			local t1, t2 = table.unpack(e.tris)
			local dot = t1.normal:dot(t2.normal) 
			if dot <= cosAngleThreshold then
				local v1 = mesh.vtxs.v[mesh.triIndexes.v[3*(t2.index-1) + e.triVtxIndexes[2]-1]].pos
				local v2 = mesh.vtxs.v[mesh.triIndexes.v[3*(t2.index-1) + e.triVtxIndexes[2]%3]].pos
				local t2edge = v2 - v1
				local t2plane = t2.normal:cross(t2edge)
				-- get the vectors along each triangles (perpendicular to the normals)
				-- if t1's normal dot t2's plane vector > 0 then it's an internal angle
				-- else it's an external angle
				if t1.normal:dot(t2plane) > 0 then
					gl.glColor3f(0,1,0)
				else
					gl.glColor3f(0,0,1)
				end
				-- neither class matters, just clip along the plane made from the edge vec cross the avg of the two normals

				-- pick one of the two edges.  either will produce the same line ray 
				-- use intervals for start/finish along edge
				local s0 = math.max(e.intervals[1][1], e.intervals[2][1])
				local s1 = math.min(e.intervals[1][2], e.intervals[2][2])
				local v1 = e.planePos + e.plane.n * s0
				local v2 = e.planePos + e.plane.n * s1
				v1 = v1 + (t1.normal + t2.normal) * 1e-3
				v2 = v2 + (t1.normal + t2.normal) * 1e-3
				gl.glVertex3fv(v1.s)
				gl.glVertex3fv(v2.s)

--[[ 
				local centerPt = e.planePos + e.plane.n * ((s1 - s0) * .5)
				local normAvg = (t1.normal + t2.normal):normalize()
				gl.glColor3f(1,1,0)
				gl.glVertex3fv(centerPt.s)
				gl.glVertex3fv((centerPt + normAvg * ((s1 - s0) * .5)).s)
--]]
			end
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

return {
	unwrapUVs = unwrapUVs,
	drawUnwrapUVGraph = drawUnwrapUVGraph,
	drawUnwrapUVEdges = drawUnwrapUVEdges,
}
