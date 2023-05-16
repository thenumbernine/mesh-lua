-- from https://github.com/mapbox/earcut
local class = require 'ext.class'
local table = require 'ext.table'

local function compareX(a, b)
	return a.x < b.x
end

local Node = class()

function Node:init(i, x, y)
	-- vertex index in coordinates array
	self.i = i

	-- vertex coordinates
	self.x = x
	self.y = y

	-- previous and next vertex nodes in a polygon ring
	self.prev = nil
	self.next = nil

	-- z-order curve value
	self.z = 0

	-- previous and next nodes in z-order
	self.prevZ = nil
	self.nextZ = nil

	-- indicates whether self is a steiner point
	self.steiner = false
end

local function removeNode(p)
	p.next.prev = p.prev
	p.prev.next = p.next

	if p.prevZ then p.prevZ.nextZ = p.nextZ end
	if p.nextZ then p.nextZ.prevZ = p.prevZ end
end

-- create a node and optionally link it with previous one (in a circular doubly linked list)
local function insertNode(i, x, y, last)
	local p = Node(i, x, y)

	if not last then
		p.prev = p
		p.next = p
	else
		p.next = last.next
		p.prev = last
		last.next.prev = p
		last.next = p
	end
	return p
end

-- check if a point lies within a convex triangle
local function pointInTriangle(ax, ay, bx, by, cx, cy, px, py)
	return (cx - px) * (ay - py) >= (ax - px) * (cy - py)
	and (ax - px) * (by - py) >= (bx - px) * (ay - py)
	and (bx - px) * (cy - py) >= (cx - px) * (by - py)
end

-- signed area of a triangle
local function area(p, q, r)
	return (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
end

local function sign(num)
	return num > 0 and 1 or (num < 0 and -1 or 0)
end

-- for collinear points p, q, r, check if point q lies on segment pr
local function onSegment(p, q, r)
	return q.x <= math.max(p.x, r.x)
	and q.x >= math.min(p.x, r.x)
	and q.y <= math.max(p.y, r.y)
	and q.y >= math.min(p.y, r.y)
end

-- check if two segments intersect
local function intersects(p1, q1, p2, q2)
	local o1 = sign(area(p1, q1, p2))
	local o2 = sign(area(p1, q1, q2))
	local o3 = sign(area(p2, q2, p1))
	local o4 = sign(area(p2, q2, q1))
	if o1 ~= o2 and o3 ~= o4 then return true end -- general case
	if o1 == 0 and onSegment(p1, p2, q1) then return true end -- p1, q1 and p2 are collinear and p2 lies on p1q1
	if o2 == 0 and onSegment(p1, q2, q1) then return true end -- p1, q1 and q2 are collinear and q2 lies on p1q1
	if o3 == 0 and onSegment(p2, p1, q2) then return true end -- p2, q2 and p1 are collinear and p1 lies on p2q2
	if o4 == 0 and onSegment(p2, q1, q2) then return true end -- p2, q2 and q1 are collinear and q1 lies on p2q2
	return false
end

-- check if a polygon diagonal is locally inside the polygon
local function locallyInside(a, b)
	if area(a.prev, a, a.next) < 0 then
		return area(a, b, a.next) >= 0 
		and area(a, a.prev, b) >= 0
	else
		return area(a, b, a.prev) < 0
		or area(a, a.next, b) < 0
	end
end

-- check if two points are equal
local function equals(p1, p2)
	return p1.x == p2.x and p1.y == p2.y
end

-- eliminate colinear or duplicate points
local function filterPoints(start, iend)
	if not start then return start end
	if not iend then iend = start end

	local p = start
	local again
	repeat
		again = false
		if not p.steiner and (equals(p, p.next) or area(p.prev, p, p.next) == 0) then
			removeNode(p)
			iend = p.prev
			p = p.prev
			if p == p.next then break end
			again = true
		else
			p = p.next
		end
	until not (again or p ~= iend)
	return iend
end

-- go through all polygon nodes and cure small local self-intersections
local function cureLocalIntersections(start, triangles, dim)
	local p = start
	repeat
		local a = p.prev
		local b = p.next.next
		if not equals(a, b)
		and intersects(a, p, p.next, b)
		and locallyInside(a, b) 
		and locallyInside(b, a)
		then
			triangles:insert(1 + math.floor((a.i-1) / dim))
			triangles:insert(1 + math.floor((p.i-1) / dim))
			triangles:insert(1 + math.floor((b.i-1) / dim))
			-- remove two nodes involved
			removeNode(p)
			removeNode(p.next)
			p = b
			start = b
		end
		p = p.next
	until p == start
	return filterPoints(p)
end

local function signedArea(data, start, iend, dim)
	local sum = 0
	local j = iend - dim
	for i = start,iend,dim do
		sum = sum + (data[j] - data[i]) * (data[i + 1] + data[j + 1])
		j = i
	end
	return sum
end

-- create a circular doubly linked list from polygon points in the specified winding order
local function linkedList(data, start, iend, dim, clockwise)
	local last
	if clockwise == (signedArea(data, start, iend, dim) > 0) then
		for i=start,iend,dim do
			last = insertNode(i, data[i], data[i+1], last)
		end
	else
		for i=iend-dim+1,start,-dim do
			last = insertNode(i, data[i], data[i+1], last)
		end
	end
	if last and equals(last, last.next) then
		removeNode(last)
		last = last.next
	end
	return last
end

-- check whether a polygon node forms a valid ear with adjacent nodes
local function isEar(ear)
	local a = ear.prev
	local b = ear
	local c = ear.next
	if area(a, b, c) >= 0 then return false end -- reflex, can't be an ear
	-- now make sure we don't have other points inside the potential ear
	-- triangle bbox; min & max are calculated like this for speed
	local x0 = math.min(a.x, b.x, c.x)
	local y0 = math.min(a.y, b.y, c.y)
	local x1 = math.max(a.x, b.x, c.x)
	local y1 = math.max(a.y, b.y, c.x)
	local p = c.next
	while p ~= a do
		if p.x >= x0 and p.x <= x1 and p.y >= y0 and p.y <= y1
		and pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) 
		and area(p.prev, p, p.next) >= 0
		then
			return false
		end
		p = p.next
	end
	return true
end

-- z-order of a point given coords and inverse of the longer side of data bbox
local function zOrder(x, y, minX, minY, invSize)
	assert(invSize)
	-- coords are transformed into non-negative 15-bit integer range
	x = math.floor((x - minX) * invSize)
	y = math.floor((y - minY) * invSize)

	x = bit.band(bit.bor(x, bit.lshift(x, 8)), 0x00FF00FF)
	x = bit.band(bit.bor(x, bit.lshift(x, 4)), 0x0F0F0F0F)
	x = bit.band(bit.bor(x, bit.lshift(x, 2)), 0x33333333)
	x = bit.band(bit.bor(x, bit.lshift(x, 1)), 0x55555555)

	y = bit.band(bit.bor(y, bit.lshift(y, 8)), 0x00FF00FF)
	y = bit.band(bit.bor(y, bit.lshift(y, 4)), 0x0F0F0F0F)
	y = bit.band(bit.bor(y, bit.lshift(y, 2)), 0x33333333)
	y = bit.band(bit.bor(y, bit.lshift(y, 1)), 0x55555555)

	return bit.bor(x, bit.lshift(y, 1))
end

local function isEarHashed(ear, minX, minY, invSize)
	assert(invSize)
	local a = ear.prev
	local b = ear
	local c = ear.next
	if area(a, b, c) >= 0 then return false end -- reflex, can't be an ear
	-- triangle bbox; min & max are calculated like this for speed
	local x0 = math.min(a.x, b.x, c.x)
	local y0 = math.min(a.y, b.y, c.y)
	local x1 = math.max(a.x, b.x, c.x)
	local y1 = math.max(a.y, b.y, c.y)
	-- z-order range for the current triangle bbox;
	local minZ = zOrder(x0, y0, minX, minY, invSize)
	local maxZ = zOrder(x1, y1, minX, minY, invSize)
	local p = ear.prevZ
	local n = ear.nextZ
	-- look for points inside the triangle in both directions
	while p and p.z >= minZ and n and n.z <= maxZ do
		if p.x >= x0 and p.x <= x1 and p.y >= y0 and p.y <= y1 and p ~= a and p ~= c and
			pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) and area(p.prev, p, p.next) >= 0
		then
			return false
		end
		p = p.prevZ
		if n.x >= x0 and n.x <= x1 and n.y >= y0 and n.y <= y1 and n ~= a and n ~= c and
			pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, n.x, n.y) and area(n.prev, n, n.next) >= 0
		then
			return false
		end
		n = n.nextZ
	end
	-- look for remaining points in decreasing z-order
	while p and p.z >= minZ do
		if p.x >= x0 and p.x <= x1 and p.y >= y0 and p.y <= y1 and p ~= a and p ~= c and
			pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) and area(p.prev, p, p.next) >= 0
		then
			return false
		end
		p = p.prevZ
	end
	-- look for remaining points in increasing z-order
	while n and n.z <= maxZ do
		if n.x >= x0 and n.x <= x1 and n.y >= y0 and n.y <= y1 and n ~= a and n ~= c and
			pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, n.x, n.y) and area(n.prev, n, n.next) >= 0
		then
			return false
		end
		n = n.nextZ
	end
	return true
end

-- try splitting polygon into two and triangulate them independently
local function splitEarcut(start, triangles, dim, minX, minY, invSize)
	-- look for a valid diagonal that divides the polygon into two
	local a = start
	repeat
		local b = a.next.next
		while b ~= a.prev do
			if a.i ~= b.i 
			and isValidDiagonal(a, b)
			then
				-- split the polygon in two by the diagonal
				local c = splitPolygon(a, b)
				-- filter colinear points around the cuts
				a = filterPoints(a, a.next)
				c = filterPoints(c, c.next)
				-- run earcut on each half
				earcutLinked(a, triangles, dim, minX, minY, invSize, 0)
				earcutLinked(c, triangles, dim, minX, minY, invSize, 0)
				return
			end
			b = b.next
		end
		a = a.next
	until a == start
end

-- Simon Tatham's linked list merge sort algorithm
-- http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
local function sortLinked(list)
	local i, p, q, e, tail, numMerges, pSize, qSize
	local inSize = 1
	repeat
		p = list
		list = nil
		tail = nil
		numMerges = 0
		while p do
			numMerges = numMerges + 1
			q = p
			pSize = 0
			for i=1,inSize do
				pSize = pSize + 1
				q = q.nextZ
				if not q then break end
			end
			qSize = inSize
			while pSize > 0 or (qSize > 0 and q) do
				if pSize ~= 0 and (qSize == 0 or not q or p.z <= q.z) then
					e = p
					p = p.nextZ
					pSize = pSize - 1
				else
					e = q
					q = q.nextZ
					qSize = qSize - 1
				end
				if tail then
					tail.nextZ = e
				else
					list = e
				end
				e.prevZ = tail
				tail = e
			end
			p = q
		end
		tail.nextZ = nil
		inSize = inSize * 2
	until numMerges <= 1
	return list
end

-- interlink polygon nodes in z-order
local function indexCurve(start, minX, minY, invSize)
	assert(invSize)
	local p = start
	repeat
		if p.z == 0 then p.z = zOrder(p.x, p.y, minX, minY, invSize) end
		p.prevZ = p.prev
		p.nextZ = p.next
		p = p.next
	until p == start
	p.prevZ.nextZ = nil
	p.prevZ = nil
	sortLinked(p)
end

-- main ear slicing loop which triangulates a polygon (given as a linked list)
local function earcutLinked(ear, triangles, dim, minX, minY, invSize, pass)
	if not ear then return end
	-- interlink polygon nodes in z-order
	if not pass and invSize then indexCurve(ear, minX, minY, invSize) end
	local stop = ear
	-- iterate through ears, slicing them one by one
	while ear.prev ~= ear.next do
		local prev = ear.prev
		local next = ear.next
		local test
		if invSize then
			test = isEarHashed(ear, minX, minY, invSize)
		else
			test = isEar(ear)
		end
		if test then
			-- cut off the triangle
			triangles:insert(1 + math.floor((prev.i-1) / dim))
			triangles:insert(1 + math.floor((ear.i-1) / dim))
			triangles:insert(1 + math.floor((next.i-1) / dim))
			removeNode(ear)
			-- skipping the next vertex leads to less sliver triangles
			ear = next.next
			stop = next.next
		else
			ear = next
			-- if we looped through the whole remaining polygon and can't find any more ears
			if ear == stop then
				-- try filtering points and slicing again
				if not pass then
					earcutLinked(filterPoints(ear), triangles, dim, minX, minY, invSize, 1)
				-- if this didn't work, try curing all small self-intersections locally
				elseif pass == 1 then
					ear = cureLocalIntersections(filterPoints(ear), triangles, dim)
					earcutLinked(ear, triangles, dim, minX, minY, invSize, 2)
				-- as a last resort, try splitting the remaining polygon into two
				elseif pass == 2 then
					splitEarcut(ear, triangles, dim, minX, minY, invSize)
				end
				break
			end
		end
	end
end

-- find a bridge between vertices that connects hole with an outer ring and and link it
local function eliminateHole(hole, outerNode)
	local bridge = findHoleBridge(hole, outerNode)
	if not bridge then return outerNode end
	local bridgeReverse = splitPolygon(bridge, hole)
	-- filter collinear points around the cuts
	filterPoints(bridgeReverse, bridgeReverse.next)
	return filterPoints(bridge, bridge.next)
end

-- whether sector in vertex m contains sector in vertex p in the same coordinates
local function sectorContainsSector(m, p)
	return area(m.prev, m, p.prev) < 0
	and area(p.next, m, m.next) < 0
end

-- David Eberly's algorithm for finding a bridge between hole and outer polygon
local function findHoleBridge(hole, outerNode)
	local p = outerNode
	local hx = hole.x
	local hy = hole.y
	local qx = -math.huge
	
	local m
	-- find a segment intersected by a ray from the hole's leftmost point to the left;
	-- segment's endpoint with lesser x will be potential connection point
	repeat
		if hy <= p.y
		and hy >= p.next.y
		and p.next.y ~= p.y
		then
			local x = p.x + (hy - p.y) * (p.next.x - p.x) / (p.next.y - p.y)
			if x <= hx and x > qx then
				qx = x
				m = p.x < p.next.x and p or p.next
				if x == hx then return m end -- hole touches outer segment; pick leftmost endpoint
			end
		end
		p = p.next
	until p == outerNode
	if not m then return end

	-- look for points inside the triangle of hole point, segment intersection and endpoint;
	-- if there are no points found, we have a valid connection;
	-- otherwise choose the point of the minimum angle with the ray as connection point

	local stop = m
	local mx = m.x
	local my = m.y
	local tanMin = math.huge
	local tan
	p = m
	repeat
		if hx >= p.x
		and p.x >= mx
		and hx ~= p.x 
		and pointInTriangle(hy < my and hx or qx, hy, mx, my, hy < my and qx or hx, hy, p.x, p.y)
		then
			tan = math.abs(hy - p.y) / (hx - p.x) -- tangential
			if locallyInside(p, hole)
			and (
				tan < tanMin
				or (
					tan == tanMin
					and (
						p.x > m.x
						or (
							p.x == m.x
							and sectorContainsSector(m, p)
						)
					)
				)
			)
			then
				m = p
				tanMin = tan
			end
		end
		p = p.next
	until p == stop
	return m
end

-- link every hole into the outer loop, producing a single-ring polygon without holes
local function eliminateHoles(data, holeIndices, outerNode, dim)
	local queue = table()
	for i=1,#holeIndices do
		local start = holeIndices[i] * dim
		local iend = i < #holeIndicies and holeIndices[i + 1] * dim or #data
		local list = linkedList(data, start, iend, dim, false)
		if list == list.next then list.steiner = true end
		queue:insert(getLeftmost(list))
	end
	queue:sort(compareX)
	-- process holes from left to right
	for i=1,#queue do
		outerNode = eliminateHole(queue[i], outerNode)
	end
	return outerNode
end

-- find the leftmost node of a polygon ring
local function getLeftmost(start)
	local p = start
	local leftmost = start
	repeat
		if p.x < leftmost.x or (p.x == leftmost.x and p.y < leftmost.y) then
			leftmost = p
		end
		p = p.next
	until p == start
	return leftmost
end

-- check if a diagonal between two polygon nodes is valid (lies in polygon interior)
local function isValidDiagonal(a, b)
	return a.next.i ~= b.i 
	and a.prev.i ~= b.i 
	and not intersectsPolygon(a, b)  -- dones't intersect other edges
	and (
		locallyInside(a, b) 
		and locallyInside(b, a) 
		and middleInside(a, b)-- locally visible
		and (
			area(a.prev, a, b.prev)
			or area(a, b.prev, b)
		) 
		or equals(a, b)-- does not create opposite-facing sectors
		and area(a.prev, a, a.next) > 0 
		and area(b.prev, b, b.next) > 0
	) -- special zero-length case
end

-- check if a polygon diagonal intersects any polygon segments
local function intersectsPolygon(a, b)
	local p = a
	repeat
		if p.i ~= a.i
		and p.next.i ~= a.i
		and p.i ~= b.i
		and p.next.i ~= b.i
		and intersects(p, p.next, a, b)
		then
			return true
		end
		p = p.next
	until p == a

	return false
end

-- check if the middle point of a polygon diagonal is inside the polygon
local function middleInside(a, b)
	local p = a
	local inside = false
	local px = (a.x + b.x) / 2
	local py = (a.y + b.y) / 2
	repeat
		if (p.y > py ~= p.next.y > py)
		and p.next.y ~= p.y
		and px < (p.next.x - p.x) * (py - p.y) / (p.next.y - p.y) + p.x
		then
			inside = not inside
		end
		p = p.next
	until p == a

	return inside
end

-- link two polygon vertices with a bridge; if the vertices belong to the same ring, it splits polygon into two;
-- if one belongs to the outer ring and another to a hole, it merges it into a single ring
local function splitPolygon(a, b)
	local a2 = Node(a.i, a.x, a.y)
	local b2 = Node(b.i, b.x, b.y)
	local an = a.next
	local bp = b.prev

	a.next = b
	b.prev = a

	a2.next = an
	an.prev = a2

	b2.next = a2
	a2.prev = b2

	bp.next = b2
	b2.prev = bp

	return b2
end

-- return a percentage difference between the polygon area and its triangulation area
-- used to verify correctness of triangulation
local function deviation(data, holeIndices, dim, triangles)
	local hasHoles = holeIndices and #holeIndices
	local outerLen = hasHoles and holeIndices[1] * dim or #data

	local polygonArea = math.abs(signedArea(data, 0, outerLen, dim))
	if hasHoles then
		local len = #holeIndices
		for i=1,len do
			local start = holeIndices[i] * dim
			local iend = i < len and holeIndices[i + 1] * dim or #data
			polygonArea = polygonArea - math.abs(signedArea(data, start, iend, dim))
		end
	end

	local trianglesArea = 0
	for i=1,#triangles,3 do
		local a = (triangles[i] - 1) * dim + 1
		local b = (triangles[i + 1] - 1) * dim + 1
		local c = (triangles[i + 2] - 1) * dim + 1
		trianglesArea = trianglesArea  + math.abs(
			(data[a] - data[c]) * (data[b + 1] - data[a + 1]) -
			(data[a] - data[b]) * (data[c + 1] - data[a + 1]))
	end

	return polygonArea == 0
	and trianglesArea == 0
	and 0
	or math.abs((trianglesArea - polygonArea) / polygonArea)
end

-- turn a polygon in a multi-dimensional array form (e.g. as in GeoJSON) into a form Earcut accepts
local function flatten(data)
	local dim = #data[1][1]
	local result = {
		vertices = table(),
		holes = {},
		dimensions = dim,
	}
	local holeIndex = 1

	for i=1,#data do
		for j=1,#data[i] do
			for d=1,dim do
				result.vertices:insert(data[i][j][d])
			end
		end
		if i > 1 then
			holeIndex = holeIndex + #data[i - 1]
			result.holes:insert(holeIndex)
		end
	end
	return result
end

local function earcut(data, holeIndices, dim)
	dim = dim or 2
	local hasHoles = holeIndices and #holeIndices > 0
	local outerLen = hasHoles and holeIndices[1] * dim or #data
	local outerNode = linkedList(data, 1, outerLen, dim, true)
	local triangles = table()
	if not outerNode or outerNode.next == outerNode.prev then return triangles end
	if hasHoles then outerNode = eliminateHoles(data, holeIndices, outerNode, dim) end
	-- if the shape is not too simple, we'll use z-order curve hash later; calculate polygon bbox
	local minX, minY, maxX, maxY
	local invSize
	if #data > 80 * dim then
		minX = data[1]
		maxX = data[1]
		minY = data[2]
		maxY = data[2]

		for i=dim+1,outerLen,dim do
			local x = data[i]
			local y = data[i + 1]
			minX = math.min(minX, x)
			maxX = math.max(maxX, x)
			minY = math.min(minY, y)
			maxY = math.max(maxY, y)
		end
		-- minX, minY and invSize are later used to transform coords into integers for z-order calculation
		invSize = math.max(maxX - minX, maxY - minY)
		if invSize ~= 0 then
			invSize = 32767 / invSize
		else
			invSize = nil
		end
	end
	earcutLinked(outerNode, triangles, dim, minX, minY, invSize, 0)
	return triangles
end

return earcut
