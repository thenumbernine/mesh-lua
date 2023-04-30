-- this belongs in its own place, outside this project


local function unwrapUVs(mesh)
-- TODO put this all in its own function or its own app
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
	local avgNormal = matrix{0,0,0}
	for _,t in ipairs(mesh.tris) do
		avgNormal = avgNormal + t.normal * t.area
	end
	local avgNormalIsZero = avgNormal:normSq() < 1e-7
	if not avgNormalIsZero then avgNormal = avgNormal:normalize() end
	print('avg normal = '..avgNormal)

	-- the same idea as the l=1 spherical harmonics
	local range = require 'ext.range'
	local areas = matrix{6}:zeros()
	for _,t in ipairs(mesh.tris) do
		local _,i = table.sup(t.normal:map(math.abs))
		assert(i)
		local dir = t.normal[i] > 0 and 1 or 2
		local index = dir + 2 * (i-1)
		areas[index] = areas[index] + t.area
	end
	print('per-side x plus/minus normal distribution = '..require 'ext.tolua'(areas))

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
	local function getEdgeOppositeTri(e, t)
		assert(#e.tris == 2)
		local t1,t2 = table.unpack(e.tris)
		if t2 == t then
			t1, t2 = t2, t1
		end
		assert(t1 == t)
		return t2, t1
	end
	local function calcUVBasis(t, tsrc, esrc)
		assert(not t[1].uv and not t[2].uv and not t[3].uv)
		-- t[1] is our origin
		-- t[1]->t[2] is our x axis with unit length
		local v = matrix{3,3}:lambda(function(i,j) return mesh.vs[t[i].v][j] end)
--print('v\n'..v)
		local d1 = v[2] - v[1]
		local d2 = v[3] - v[2]
		local n = d1:cross(d2)
		local nlen = n:norm()
--print('|d1 x d2| = '..nlen)
		if not math.isfinite(nlen)
		or nlen < 1e-9
		then
			t.normal = d1:normalize()
			-- can't fold this because i'ts not a triangle ... it's a line
			-- should I even populate the uv fields?  nah, just toss it in the caller
			return true
		end
		n = n / nlen
--print('n = '..n)
		t.normal = matrix(n)

		--if true then
		if not tsrc then	-- first basis
			t.uvorigin2D = matrix{0,0}
			-- modularity for choosing which point on the tri is the uv origin
			--[[ use the first point
			t.uvorigin3D = matrix(v[1])
			--]]
			-- [[ use the y-lowest point
			-- good for roofs ... not for walls ...
			t.uvorigin3D = matrix(v[
				select(2, range(3):mapi(function(i)
					return v[i][2]
				end):inf())
			])
			mesh.unwrapUVOrigins:insert(t.uvorigin3D * .7 + t.com * .3)
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
			if math.abs(d1[2]) < math.abs(d2[2]) then	-- d1 < d2
				if math.abs(d1[2]) < math.abs(d3[2]) then	-- d1 < d2 and d1 < d3
					ex = d1:normalize()
				else			-- d3 < d1 < d2
					ex = d3:normalize()
				end
			else	-- d2 < d1
				if math.abs(d2[2]) < math.abs(d3[2]) then	-- d2 < d1 and d2 < d3
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
			--[[ just use n cross y+
			-- BEST FOR CARTESIAN ALIGNED
			-- best for top
			-- crashes for sides
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
			local a = matrix{0,0,0}
			a[i] = 1
			local ex = n:cross(a):normalize()
			assert(math.isfinite(ex:normSq()))
			--]]
			--[[ draw a line between the lowest two points
			if v[1][2] > v[2][2] then
				if v[1][2] > v[3][2] then	-- 1 highest
					ex = (v[3] - v[2]):normalize()
				else	-- 3 highest
					ex = (v[2] - v[1]):normalize()
				end
			else
				if v[2][2] > v[3][2] then	-- 2 highest
					ex = (v[1] - v[3]):normalize()
				else	-- 3 highest
					ex = (v[2] - v[1]):normalize()
				end
			end
			--]]
			-- [[ most orthogonal to bestNormal take 2
			local d3 = v[1] - v[3]
			local ds = table{d1:normalize(), d2:normalize(), d3:normalize()}
			local dots = ds:mapi(function(d) return math.abs(d:dot(bestNormal)) end)
			local i = range(3):sort(function(a,b) return dots[a] < dots[b] end)[1]
			local ex = ds[i]
			ex = n:cross(ex):normalize()
			--]]

			-- fallback, if n is nan or zero
			local exNormSq = ex:normSq()
			if exNormSq < 1e-3						-- can't use zero
			or not math.isfinite(exNormSq)			-- can't use nan
			or math.abs(ex:dot(n)) > 1 - 1e-3	-- can't use ex perp to n
			then
print('failed to find u vector based on bestNormal, picked ex='..ex..' from bestNormal '..bestNormal)
				-- pick any basis perpendicular to 'n'
				local ns = matrix{3}:lambda(function(i)
					local a = matrix{0,0,0}
					a[i] = 1
					return n:cross(a)
				end)
--print('choices\n'..ns)
				local lens = matrix{3}:lambda(function(i) return ns[i]:normSq() end)
				local _, i = table.sup(lens)	-- best normal
--print('biggest cross '..i)
				ex = ns[i]:normalize()
print('picking fallback ', ex)
			end

--print('ex = '..ex)
			-- tangent space.  store as row vectors i.e. transpose, hence the T
			t.uvbasisT = matrix{
				ex,
				n:cross(ex):normalize(),
				n,
			}
--print('ey = '..t.uvbasisT[2])
		else
			assert(tsrc[1].uv and tsrc[2].uv and tsrc[3].uv)

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
			if tsrc[i11].uv then
				isrc = i11
			elseif tsrc[i12].uv then
				isrc = i12
			else
				error("how can we fold a line when the src tri doesn't have uv coords for it?")
			end
			t.uvorigin2D = matrix(tsrc[isrc].uv)			-- copy matching uv from edge neighbor
			t.uvorigin3D = matrix(mesh.vs[tsrc[isrc].v])	-- copy matching 3D position
--print('uv2D = '..t.uvorigin2D)
--print('uv3D = '..t.uvorigin3D)

			-- modularity for choosing unwrap rotation
			--[[ reset basis every time. dumb.
			local ex = d1:normalize()
			t.uvbasisT = matrix{
				ex,
				n:cross(ex):normalize(),
				n,
			}
			--]]
			--[[ subsequent tri basis should be constructed from rotating the prev tri basis
			-- find the rotation from normal 1 to normal 2
			-- that'll just be the matrix formed from n1 and n2's basis ...
			local q = quat():vectorRotate(tsrc.normal, t.normal)
			t.uvbasisT = matrix{
				q:rotate(tsrc.uvbasisT[1]),
				q:rotate(tsrc.uvbasisT[2]),
				n,
			}
			--]]
			-- [[ pick the rotation along the cardinal axis that has the greatest change
			-- BEST FOR CARTESIAN ALIGNED
			local dn = t.normal - tsrc.normal
			local q
			if dn:normSq() < 1e-3 then
				q = quat(0,0,0,1)
			else
				-- pick smallest changing axis in normal?
				local _, i = table.inf(dn:map(math.abs))
				if i == 1 then
					local degrees = math.deg(math.atan2(n[3], n[2]) - math.atan2(tsrc.normal[3], tsrc.normal[2]))
--print(t.normal, tsrc.normal, dn, 'rot on x-axis by', degrees)
					q = quat():fromAngleAxis(1, 0, 0, degrees)
				elseif i == 2 then
					local degrees = math.deg(math.atan2(n[1], n[3]) - math.atan2(tsrc.normal[1], tsrc.normal[3]))
--print(t.normal, tsrc.normal, dn, 'rot on y-axis by', degrees)
					q = quat():fromAngleAxis(0, 1, 0, degrees)
				elseif i == 3 then
					local degrees = math.deg(math.atan2(n[2], n[1]) - math.atan2(tsrc.normal[2], tsrc.normal[1]))
--print(t.normal, tsrc.normal, dn, 'rot on z-axis by', degrees)
					q = quat():fromAngleAxis(0, 0, 1, degrees)
				end
			end
--print('q', q)
--print('n', n)
--print('tsrc ex = '..tsrc.uvbasisT[1])
--print('tsrc ey = '..tsrc.uvbasisT[2])
			t.uvbasisT = matrix{
				q:rotate(tsrc.uvbasisT[1]),
				q:rotate(tsrc.uvbasisT[2]),
				n,
			}
			--]]

--print('|ez-n| = '..matrix(q:rotate(tsrc.uvbasisT[3]) - n):norm())
--print('ex = '..t.uvbasisT[1])
--print('ey = '..t.uvbasisT[2])
		end

		for i=1,3 do
			local d = v[i] - t.uvorigin3D
			local m = matrix{t.uvbasisT[1], t.uvbasisT[2]}
--print('d = '..d)
--print('m\n'..m)
--print('m * d = '..(m * d))
			t[i].uv = m * d + t.uvorigin2D
--print('uv = '..t[i].uv)
			if not math.isfinite(t[i].uv:normSq()) then
				print('tri has nans in its basis')
			end
		end
	end

	mesh.unwrapUVOrigins = table()
	mesh.unwrapUVEdges = table()	-- keep track of how it's made for visualization's sake ...

	local notDoneYet = table(mesh.tris)
	local done = table()

	local function calcUVBasisAndAddNeighbors(t, tsrc, e, todo)
		if tsrc then mesh.unwrapUVEdges:insert{tsrc, t} end
		-- calc the basis by rotating it around the edge
		assert((tsrc == nil) == (e == nil))
		local gotBadTri = calcUVBasis(t, tsrc, e)
		-- TODO roof actually looks good with always retarting ... but not best
		if not gotBadTri then
			done:insert(t)
			assert(t[1].uv and t[2].uv and t[3].uv)
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
							assert(not t2[1].uv and not t2[2].uv and not t2[3].uv)
							notDoneYet:remove(i)
							todo:insert(t2)
						end
					end
				end
			end
		end
	end

	local function floodFillMatchingNormalNeighbors(t, tsrc, e, alreadyFilled)
		alreadyFilled:insertUnique(t)
		if t[1].uv then return end
		if tsrc then mesh.unwrapUVEdges:insert{tsrc, t, floodFill=true} end
		assert((tsrc == nil) == (e == nil))
		if not calcUVBasis(t, tsrc, e) then
			done:insert(t)
			assert(t[1].uv and t[2].uv and t[3].uv)
			for _,e in ipairs(t.allOverlappingEdges) do
				if #e.tris == 2 then
					local t2 = getEdgeOppositeTri(e, t)
					if not alreadyFilled:find(t2) then
						if t.normal:dot(t2.normal) > 1 - 1e-3 then
							floodFillMatchingNormalNeighbors(t2, t, e, alreadyFilled)
						else
							alreadyFilled:insertUnique(t)
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
				mesh.vs[a[1].v][2],
				mesh.vs[a[2].v][2],
				mesh.vs[a[3].v][2]
			) < math.min(
				mesh.vs[b[1].v][2],
				mesh.vs[b[2].v][2],
				mesh.vs[b[3].v][2]
			)
		end)
		local todo = table{notDoneYet:remove(1)}
		--]]
		--[[ same as above but pick the lowest *edge* , not *vtx*, cuz we want the base edges aligned with the bottom
		notDoneYet:sort(function(a,b)
			local aEdgeYMin = matrix{3}:lambda(function(i)
				return .5 * (mesh.vs[a[i].v][2] + mesh.vs[a[i%3+1].v][2])
			end):min()
			local bEdgeYMin = matrix{3}:lambda(function(i)
				return .5 * (mesh.vs[b[i].v][2] + mesh.vs[b[i%3+1].v][2])
			end):min()
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
			return mesh.vs[a][2] < mesh.vs[b][2]	-- sort by y axis
		end)
		local eps = (mesh.bbox.max[2] - mesh.bbox.min[2]) * 1e-5
		local ymin = mesh.vs[vtxsNotDoneYet[1]][2]
		print('y min', ymin)
		-- now go thru all tris not done yet
		-- if any have 2/3 vtxs at the min then add them
		local todo = table()
		for i=#notDoneYet,1,-1 do
			local t = notDoneYet[i]
			local mincount = 0
			for j=1,3 do
				if mesh.vs[t[j].v][2] < ymin + eps then mincount = mincount + 1 end
			end
			if mincount >= 2 then
				todo:insert(notDoneYet:remove(i))
			end
		end
		-- if none were added then add one with 1/3 vtxs at the min
		if #todo == 0 then
			for i=#notDoneYet,1,-1 do
				local t = notDoneYet[i]
				for j=1,3 do
					if mesh.vs[t[j].v][2] < ymin + eps then
						todo:insert(notDoneYet:remove(i))
						break
					end
				end
				if #todo > 0 then break end
			end
		end
print('number to initialize with', #todo)
		-- ... and process them all once first, adding their neigbors to the 'todo' pile
		--]=]
		-- [=[ choose tris with any edges that are 90' from the guide normal
		-- but not if the vector from the com to the edges is towards the guide normal
		local todo = table()
		for i=#notDoneYet,1,-1 do
			local t = notDoneYet[i]
			for j=1,3 do
				local a = mesh.vs[t[j].v]
				local b = mesh.vs[t[j%3+1].v]
				if math.abs((b - a):normalize():dot(bestNormal)) < 1e-5 then
					-- exclude tops
					if (.5 * (b + a) - t.com):dot(bestNormal) > 0 then
						notDoneYet:remove(i)
						todo:insert(t)
						break
					end
				end
			end
		end
		-- if finding a y-perpendicular downward-pointing edge was too much to ask,
		-- ... then pick one at random?
		if #todo == 0 then
--print("couldn't find any perp-to-bestNormal edges to initialize with...")
			todo:insert(notDoneYet:remove(1))
		end
		--]=]

		-- [[ first pass to make sure all the first picked are considered
		-- during this first pass, immediately fold across any identical normals
--print('starting first pass with #todo', #todo)
		for i=#todo,1,-1 do
			local t = todo:remove(i)
			-- for 't', flood-fill through anything with matching normal
			-- while flood-filling, continue adding neighbors to 'todo'
			local filled = table()
			floodFillMatchingNormalNeighbors(t, nil, nil, filled)
			for _,t in ipairs(filled) do
				if not t[1].uv then
					todo:insertUnique(t)
				end
			end
		end
--print('after first pass, #todo', #todo, '#done', #done)
		--]]

		while #todo > 0 do
			local t, tsrc, e

			-- pick best edge between any triangle in 'done' and any in 'todo'
			local edgesToCheck = table()
			for _,t in ipairs(todo) do
				for _,e in ipairs(t.allOverlappingEdges) do
					if #e.tris == 2 then
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
					local ra = a.tri.normal:dot(a.prevtri.normal)
					local rb = b.tri.normal:dot(b.prevtri.normal)
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
				assert(tsrc[1].uv and tsrc[2].uv and tsrc[3].uv)
				todo:removeObject(t)
			end
			for _,t in ipairs(todo) do
				assert(not t[1].uv and not t[2].uv and not t[3].uv)
			end
			if t then
				calcUVBasisAndAddNeighbors(t, tsrc, e, todo)
			end
		end
		for _,t in ipairs(done) do
			assert(t[1].uv and t[2].uv and t[3].uv)
		end
	end

	-- replace?
	mesh.vts = table()
	for i=1,#mesh.tris do
		local t = mesh.tris[i]
		for j=1,3 do
			local src = t[j].uv or {0,0}
			mesh.vts:insert(matrix{src[1], src[2], 0})
			t[j].vt = #mesh.vts
			t[j].uv = nil
		end
	end
end

function drawUVUnwrapEdges(mesh)
	local gl = require 'gl'
	local eps = 1e-3
	-- [[ show unwrap info
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for _,info in ipairs(mesh.unwrapUVEdges or {}) do
		for i,t in ipairs(info) do
			if info.floodFill == true then
				gl.glColor3f(0,0,1)
			else
				if i==1 then
					gl.glColor3f(0,1,0)
				else
					gl.glColor3f(1,0,0)
				end
			end
			gl.glVertex3f((t.com + eps * t.normal):unpack())
		end
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

return {
	unwrapUVs = unwrapUVs,
	drawUVUnwrapEdges = drawUVUnwrapEdges,
}
