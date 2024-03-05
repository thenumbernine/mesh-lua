local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local math = require 'ext.math'
local range = require 'ext.range'
local timer = require 'ext.timer'
local vector = require 'ffi.cpp.vector-lua'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local box3f = require 'vec-ffi.box3f'
local plane3f = require 'vec-ffi.plane3f'
local matrix_ffi = require 'matrix.ffi'
matrix_ffi.real = 'float'

ffi.cdef[[
typedef struct MeshVertex_t {
	vec3f_t pos;
	vec3f_t texcoord;
	vec3f_t normal;

	// per-triangle stats (duplicated 3x per-vertex)
	// TODO move this to a separate buffer
	vec3f_t com;		//com of tri containing this vertex.  only good for un-indexed drawing.
} MeshVertex_t;
]]

local Mesh = class()

-- the threshold for determining if two neighboring tris are in the same plane or not.
-- using a value as high as 5 degrees lets uv-unwrapping wrap around curves
-- this threshold is used in so many places in unwrapuv and tilemesh that I thought I'd just make it a static class variable
Mesh.angleThresholdInDeg = 5

local function triArea(a,b,c)
	-- TODO check nans here?
	local n = (b - a):cross(c - a)
	return .5 * n:norm()
end
Mesh.triArea = triArea

-- TODO the more I use this (and build off it ... giving triangle TNB frames ...)
-- the more I think I should re-introduce storing the triangle normal
local function triNormal(a,b,c)
	local n, len = (b - a):cross(c - b):unitOrZero()
	return n, len * .5
	-- returns the unit normal, triangle area
end
Mesh.triNormal = triNormal

local function triCOM(a,b,c)
	-- TODO check nans here?
	return (a + b + c) * (1/3)
end
Mesh.triCOM = triCOM

-- volume of parallelogram with vertices at 0, a, b, c
-- the 4th pt in the tetrad is zero.  adjust a,b,c accordingly
local function tetradVolume(a,b,c)
	return (a.x * b.y * c.z
		+ a.y * b.z * c.x
		+ a.z * b.x * c.y
		- c.x * b.y * a.z
		- c.y * b.z * a.x
		- c.z * b.x * a.y) / 6
end
Mesh.tetradVolume = tetradVolume

--[[ holds extra info per tri:
.index = what it's 1-based index is
.group = what its group is (unnecessary? finding via groups ranges is probably faster than finding a tri in the .tris table)
.com = triangle center-of-mass (average of 3 vtxs)
.area
.normal = tri surface normal (surface z-axis)
.basis = [3] of tangent, (negative of) binormal, normal

findEdges:
.edges

unwrapuv:
.uvs = [3] vec2f's of the uv's of the triangle (before breaking and writing)
.uvorigin2D
.uvorigin3D
--]]
local Triangle = class()

function Triangle:init(args)
	if args then
		for k,v in pairs(args) do
			self[k] = v
		end
	end
end

function Triangle:indexes(mesh)
	local ti = 3 * (self.index - 1)
	assert(ti >= 0 and ti + 3 <= mesh.triIndexes.size)
	local tp = mesh.triIndexes.v + ti
	local i,j,k = tp[0], tp[1], tp[2]
	assert(i >= 0 and i < mesh.vtxs.size)
	assert(j >= 0 and j < mesh.vtxs.size)
	assert(k >= 0 and k < mesh.vtxs.size)
	return i,j,k
end

function Triangle:vtxs(mesh)
	local i,j,k = self:indexes(mesh)
	return mesh.vtxs.v[i], mesh.vtxs.v[j], mesh.vtxs.v[k]
end

function Triangle:vtxPos(mesh)
	local va,vb,vc = self:vtxs(mesh)
	return va.pos, vb.pos, vc.pos
end

function Triangle:calcCOM(mesh)
	local a, b, c = self:vtxPos(mesh)
	self.com = (a + b + c) * (1 / 3)
end

function Triangle:calcAux(mesh)
	self:calcCOM(mesh)
	self.normal, self.area = mesh.triNormal(self:vtxPos(mesh))
end

-- calculate the barycentric coordinates of point 'p'
function Triangle:calcBCC(p, mesh)
	local ti = 3 * (self.index - 1)
	assert(ti >= 0 and ti + 3 <= mesh.triIndexes.size)
	local tp = mesh.triIndexes.v + ti
	assert(tp[0] >= 0 and tp[0] < mesh.vtxs.size)
	assert(tp[1] >= 0 and tp[1] < mesh.vtxs.size)
	assert(tp[2] >= 0 and tp[2] < mesh.vtxs.size)

	local bcc = vec3f()
	for j=0,2 do
		local v1 = mesh.vtxs.v[tp[(j+1)%3]].pos
		local v2 = mesh.vtxs.v[tp[(j+2)%3]].pos
		local v3 = mesh.vtxs.v[tp[(j+0)%3]].pos
		local vavg = .5 * (v1 + v2)
		local edgeDir = v2 - v1
		local tocom = self.normal:cross(edgeDir):normalize()
		local edgePlane = plane3f():fromDirPt(tocom, vavg)
		local oppDist = edgePlane:dist(v3)
		-- TODO rescale correctly
		-- right now they're only good for signedness test
		bcc.s[j] = (p - v1):dot(tocom) / oppDist
	end
	return bcc
end

-- returns 'true' if 'p' is inside the triangle according to barycentric coordinate test
function Triangle:insideBCC(p, mesh)
	local bcc = self:calcBCC(p, mesh)
	return bcc.x >= 0 and bcc.y >= 0 and bcc.z >= 0
end

function Triangle:calcTetradVolume(mesh)
	return tetradVolume(self:vtxPos(mesh))
end

Mesh.Triangle = Triangle

--[[
static function for generating the shader
args:
	glslVersion = version to put on the shader code.
	glslHeader = header to put on shader code.  overrides glslVersion.
--]]
function Mesh:makeShader(args)
	args = args or {}
	local GLProgram = require 'gl.program'
	local version = args.glslVersion or '460'
	local header = args.glslHeader or '#version '..version..'\n'
	return GLProgram{
		vertexCode = header..[[
in vec3 pos;
in vec3 texcoord;
in vec3 normal;
in vec3 com;

uniform bool useFlipTexture;
uniform vec4 Ka;
uniform vec4 Kd;
uniform vec4 Ks;
uniform float Ns;
uniform vec3 objCOM;
uniform vec3 groupCOM;
uniform float groupExplodeDist;
uniform float triExplodeDist;
uniform mat4 modelMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;

out vec3 viewPosv;	// position in view space
out vec3 texcoordv;
out vec3 normalv;
out vec4 Kav;
out vec4 Kdv;
out vec4 Ksv;
out float Nsv;

void main() {
	texcoordv = texcoord;
	if (useFlipTexture) texcoordv.y = 1. - texcoordv.y;
	mat4 modelViewMatrix = viewMatrix * modelMatrix;
	normalv = (modelViewMatrix * vec4(normal, 0.)).xyz;
	Kav = Ka;
	Kdv = Kd;
	Ksv = Ks;
	Nsv = Ns;
	vec3 vertex = pos;
	vertex = mix(vertex, com, triExplodeDist);
	vertex = mix(vertex, groupCOM, groupExplodeDist);
	vec4 viewPos = modelViewMatrix * vec4(vertex, 1.);
	viewPosv = viewPos.xyz;
	gl_Position = projectionMatrix * viewPos;
}
]],
		fragmentCode = header..[[

uniform sampler2D map_Kd;
uniform bool useLighting;
uniform vec3 lightDir;
uniform bool useTextures;

in vec3 viewPosv;
in vec3 texcoordv;
in vec3 normalv;
in vec4 Kav;
in vec4 Kdv;
in vec4 Ksv;
in float Nsv;

out vec4 fragColor;

void main() {
	vec3 normal = normalize(normalv);
	fragColor = Kav;
	vec4 diffuseColor = Kdv;
	if (useTextures) {
		diffuseColor *= texture(map_Kd, texcoordv.xy);
	}
	fragColor += diffuseColor;
	if (useLighting) {
		fragColor.xyz *= max(0., dot(normal, lightDir));
	}
	if (useLighting) {
		vec3 viewDir = normalize(-viewPosv);
		vec3 reflectDir = reflect(-lightDir, normal);
		float spec = pow(max(dot(viewDir, reflectDir), 0.), Nsv);
		fragColor += Ksv * spec;
	}
}
]],
		uniforms = {
			useFlipTexture = false,
			useLighting = true,
			useTextures = false,
			lightDir = {1,1,1},
			objCOM = {0,0,0},
			groupCOM = {0,0,0},
			groupExplodeDist = 0,
			triExplodeDist = 0,
			map_Kd = 0,
			Ka = {0,0,0,0},
			Kd = {1,1,1,1},
			Ks = {1,1,1,1},
			Ns = 100,
			--modelMatrix = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1},
			--viewMatrix = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1},
			--projectionMatrix = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1},
		},
	}:useNone()
end

function Mesh:init(o)
	-- TODO replace my lua-ization of cpp-vectors
	-- ...with a cdef-ization of lua-tables
	-- because everyone knows the stl api is way too longwinded compared to equiv commands in other languages/apis, and is only that way to accomodate functional programming and templates.
	self.vtxs = vector'MeshVertex_t'
	self.triIndexes = vector'int32_t'

	-- array of Triangle's
	self.tris = table()

	-- holds 0-based ranges of tris
	self.groups = table()
end

--[[
what should this class have?
all have:
	planePos (really these two are a ray, but the ray's plane is used for projection for distance calculatiosn so ...)
		- sometimes this is an endpoint, sometimes this is the average of them,
	plane = plane with normale equal to the edge direction, and point equal to planePos
	interval
	clipPlane = plane containing the edge and its normAvg
	normAvg = average of normals of all adjacent triangles
	tris = table of mesh.tris[] objects that share this edge
mesh.edges[] has:
	[i] = vertex index
	length = dist between endpoints ...
	... should be equal to the interval difference, but no promises.
	com = average of two points
	... equal to planePos for this particular edge.
mesh.edges2[] has:
	tris[] will only has 2 entries exactly.
	triVtxIndexes[] has two entries = table of indexes into edge.tris[] such that it and the tri's next (mod 3) form the edge
	dist = distance the lines are apart (based on just 1 of their 2 vtxs, lame test)
	isPlanar = true if the two tris have matching normals (within tolerance)
	isExtEdge = true for convex vs concave edges
	... planePos is the average of the endpoints of both edges.
	... no com defined, might not exactly be the avg of the two endpoints based on plane.n and planePos, because its
fakeEdges (that goes in mesh.edgeClipGroups)
	basis = used for placing meshes along them? idk?
	com = midpoint of line segment
--]]
local Edge = class()

function Edge:init(args)
	if args then
		for k,v in pairs(args) do
			self[k] = v
		end
	end
end

-- get the endpoints of the edge
function Edge:getPts()
	assert(#self.interval == 2)
	local s0, s1 = table.unpack(self.interval)
	assert(s0 <= s1)
	local v0 = self.planePos + self.plane.n * s0
	local v1 = self.planePos + self.plane.n * s1
	return v0, v1
end

-- calcs the line segment COM by its endpoints' average
function Edge:calcCOM()
	local v0, v1 = self:getPts()
	return .5 * (v0 + v1)
end


--[[
assert that the groups span the mesh indexes and do not overlap one another
and that the triangles are 1:1 with the indexes
--]]
function Mesh:assertGroups()
	-- assert tris x 3 == indexes
	assert(#self.tris*3 == self.triIndexes.size)
	-- assert sum of all group tri counts == total tri counts
	assert(self.groups:mapi(function(g) return g.triCount end):sum() == #self.tris)
	-- assert all group tri ranges are within total tri range
	for _,g in ipairs(self.groups) do
		assert(g.triFirstIndex >= 0)
		assert(g.triCount >= 0)
		assert(g.triFirstIndex + g.triCount <= #self.tris)
	end
	-- assert no gruops overlap
	for i=1,#self.groups-1 do
		local gi = self.groups[i]
		for j=i+1,#self.groups do
			local gj = self.groups[j]
			assert(gi.triFirstIndex + gi.triCount <= gj.triFirstIndex
				or gj.triFirstIndex + gj.triCount <= gi.triFirstIndex)
		end
	end
end

-- combines 'self' with other meshes.
-- operates in-place.
-- returns self
function Mesh:combine(...)
	self.mtlFilenames = table(self.mtlFilenames)

	for oi=1,select('#', ...) do
		local o = select(oi, ...)

		local firstVtx = self.vtxs.size
		self.vtxs:resize(self.vtxs.size + o.vtxs.size)
		ffi.copy(self.vtxs.v + firstVtx, o.vtxs.v, ffi.sizeof(o.vtxs.type) * o.vtxs.size)

		local firstIndex = self.triIndexes.size
		self.triIndexes:resize(self.triIndexes.size + o.triIndexes.size)
		for i=0,o.triIndexes.size-1 do
			self.triIndexes.v[firstIndex + i] = o.triIndexes.v[i] + firstVtx
		end

		local firstGroup = #self.groups+1
		self.groups:append(o.groups:mapi(function(g)
			g = table(g):setmetatable(nil)
			g.triFirstIndex = g.triFirstIndex + firstIndex/3
			return g
		end))

		self.tris:append(o.tris:mapi(function(t)
			t = Triangle(t)
			local groupIndex = o.groups:find(t.group)
			t.group = groupIndex and self.groups[firstGroup + groupIndex - 1] or nil
			return t
		end))
		for i,t in ipairs(self.tris) do
			t.index = i
		end

		self.mtlFilenames:append(o.mtlFilenames)
	end

	self.mtlFilenames = self.mtlFilenames:mapi(function(v,k,t)
		return true, v
	end):map(function(v,k,t)
		return k, #t+1
	end)

	self.edges = nil
	self.edgeIndexBuf = nil
	self.edges2 = nil
	self:unloadGL()
	return self
end

function Mesh:clone()
	local result = Mesh():combine(self)
	if self.com0 then result.com0 = vec3f(self.com0) end
	if self.com1 then result.com1 = vec3f(self.com1) end
	if self.com2 then result.com2 = vec3f(self.com2) end
	if self.com3 then result.com3 = vec3f(self.com3) end
	return result
end

-- TODO operators?  * number, * vec3f, etc?
function Mesh:scale(...)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i].pos
		for j=0,2 do
			v.s[j] = v.s[j] * select(j+1, ...)
		end
	end
	self:refreshVtxs()
	return self
end

-- TODO operators?
function Mesh:translate(...)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i].pos
		for j=0,2 do
			v.s[j] = v.s[j] + select(j+1, ...)
		end
	end
	self:refreshVtxs()
	local d = vec3f(...)
	if self.com0 then self.com0 = self.com0 + d end
	if self.com1 then self.com1 = self.com1 + d end
	if self.com2 then self.com2 = self.com2 + d end
	if self.com3 then self.com3 = self.com3 + d end
	return self
end

-- quaternion?  matrix?  angle-axis? detect?
-- quaternion for now.
function Mesh:rotate(q)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i]
		v.pos = q:rotate(v.pos)
		v.normal = q:rotate(v.normal)
	end
	if self.tris then
		for _,t in ipairs(self.tris) do
			if t.normal then
				t.normal = q:rotate(t.normal)
			end
			if t.basis then
				for i=1,3 do
					t.basis[i] = q:rotate(t.basis[i])
				end
			end
		end
	end
	-- TODO edges?
	self:refreshVtxs()
	return self
end

function Mesh:transform(xform)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i]
		local npos = xform * matrix_ffi{v.pos.x, v.pos.y, v.pos.z, 1}
		v.pos:set(npos:unpack())
		local nnormal = xform * matrix_ffi{v.normal.x, v.normal.y, v.normal.z, 0}
		v.normal = vec3f():set(nnormal:unpack()):normalize()
	end
	self:refreshVtxs()
	return self
end

function Mesh:recenter(newOrigin)
	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].pos = self.vtxs.v[i].pos - newOrigin
	end
	if self.vtxBuf then
		self.vtxBuf
			:bind()
			:updateData(0, ffi.sizeof'MeshVertex_t' * self.vtxs.size, self.vtxs.v)
			:unbind()
	end
	-- recalculate coms?  up to you...
	--self:calcCOMs()
end

function Mesh:refreshVtxs()
	if self.loadedGL then
		self.vtxBuf
			:bind()
			:updateData(0, ffi.sizeof'MeshVertex_t' * self.vtxs.size, self.vtxs.v)
			:unbind()
	end
	self.bbox = nil
	-- TODO invalidate instead of recalculate?
	--self:findEdges()
	--self:calcCOMs()
	return self
end

function Mesh:prepare()
-- [[ calculate bbox.
-- do this before merging vertexes
	self:calcBBox()
--]]
-- TODO maybe calc bounding radius? Here or later?  That takes COM, which, for COM2/COM3 takes tris.  COM1 takes edges... should COM1 consider merged edges always?  probably...

	-- store all edges of all triangles
	-- ... why?  who uses this?
	-- unwrapUVs used to but now it uses the 'edges2' structure
	-- it's used for visualization
	self:findEdges()

	-- calculate coms ...
	self:calcCOMs()
end

function Mesh:calcBBox()
	self.bbox = box3f.empty()
	for i=0,self.vtxs.size-1 do
		self.bbox:stretch(self.vtxs.v[i].pos)
	end
end

--[=[
args:
	prec = precision, default 1e-5
	posPrec = precision for testing .pos uniqueness.  nil means no precision means don't even check.
	texCoordPrec = use .texcoord to test uniqueness.
	normalPrec = use .normal to test uniqueness.
	usedVertexes = optional map {[0-based-vtx-index] = true} to flag which indexes to check

returns:
	uniquevs = lua-table holding all the unique 0-based vtx indexes
	indexToUniqueV = {[0-based-vtx-index] = 1-based index in uniquevs} = map from old (0-based c-array) to new (1-based lua-table)

it should always be the case that uniquevs[indexToUniqueV[i]] <= i
--]=]
-- _binning uses binning, which is fast, but means if two vtxs are really close but in the wrong bins then they wont' merge, which might be bad ...
function Mesh:getUniqueVtxs_binning(posPrec, texCoordPrec, normalPrec, usedVertexes)
	local function vec3ToStrPrec(v, prec)
		return tostring(v:map(function(x)
			return math.round(x / prec) * prec
		end))
	end

	-- map from the vtxs to unique indexes
	local uniquevs = table()

	-- used by tris.
	-- map from all vtxs.v[], into unique indexes
	-- rounds values to precision 'prec'
	-- keys are 0-based, values are 1-based
	local indexToUniqueV = {}

	-- maps from a key (from rounded vec3f) to uniquevs index
	-- goes a *lot* faster than the old way
	local keyToUnique = {}

	for i=0,self.vtxs.size-1 do
		if not usedVertexes or usedVertexes[i] then
			local v = self.vtxs.v[i]
			local k = table{
				posPrec and vec3ToStrPrec(v.pos, posPrec) or '',
				texCoordPrec and vec3ToStrPrec(v.texcoord, texCoordPrec) or '',
				normalPrec and vec3ToStrPrec(v.normal, normalPrec) or ''
			}:concat','
			local j = keyToUnique[k]
			if j then
				indexToUniqueV[i] = j
			else
				uniquevs:insert(i)
				keyToUnique[k] = #uniquevs
				indexToUniqueV[i] = #uniquevs
			end
		end
	end
	return uniquevs, indexToUniqueV
end
-- non-binning is slower but checks all previous vertexes for distance
function Mesh:getUniqueVtxs(posPrec, texCoordPrec, normalPrec, usedVertexes)
	-- map from the vtxs to unique indexes
	local uniquevs = table()

	-- used by tris.
	-- map from all vtxs.v[], into unique indexes
	-- rounds values to precision 'prec'
	-- keys are 0-based, values are 1-based
	local indexToUniqueV = {}

	for i=0,self.vtxs.size-1 do
		if not usedVertexes or usedVertexes[i] then
			local vi = self.vtxs.v[i]
			local foundj
			for j=0,i-1 do
				if not usedVertexes or usedVertexes[j] then
					local vj = self.vtxs.v[j]
					if (not posPrec or (vi.pos - vj.pos):norm() <= posPrec)
					and (not texCoordPrec or (vi.texcoord - vj.texcoord):norm() < texCoordPrec)
					and (not normalPrec or (vi.normal - vj.normal):norm() < normalPrec)
					then
						foundj = j
						break
					end
				end
			end
			if foundj then
--print(i..' => '..foundj..' => '..tostring(indexToUniqueV[foundj]))
				indexToUniqueV[i] = assert(indexToUniqueV[foundj])
			else
				uniquevs:insert(i)
--print(i..' => '..#uniquevs..' => '..i)
				indexToUniqueV[i] = #uniquevs
			end
		else
--print(i.." skipping")
		end
	end
	return uniquevs, indexToUniqueV
end

function Mesh:mergeMatchingVertexes(skipTexCoords, skipNormals)
	assert(#self.tris*3 == self.triIndexes.size)
	if not self.bbox then self:calcBBox() end
	-- ok the bbox hyp is 28, the smallest maybe valid dist is .077, and everything smalelr is 1e-6 ...
	-- that's a jump from 1/371 to 1/20,000,000
	-- so what's the smallest ratio I should allow?  maybe 1/1million?
--	local bboxCornerDist = (self.bbox.max - self.bbox.min):norm()
--	local vtxMergeThreshold = bboxCornerDist * 1e-6
--print('vtxMergeThreshold', vtxMergeThreshold)
--print('before merge vtx count', self.vtxs.size, 'tri count', self.triIndexes.size)
	local vtxMergeThreshold = 1e-5
	local uniquevs, indexToUniqueV = self:getUniqueVtxs(
		vtxMergeThreshold,
		not skipTexCoords and 1e-7,
		not skipNormals and 1e-7
	)
	for i=self.vtxs.size-1,1,-1 do
		local j = uniquevs[indexToUniqueV[i]]
		assert(j <= i)
		if j < i then
			self:mergeVertex(i,j)
		end
	end
--print('after merge vtx count', self.vtxs.size, 'tri count', self.triIndexes.size)
	assert(#self.tris*3 == self.triIndexes.size)

	-- invalidate
	self:unloadGL()

	self.edges = nil
	self.edgeIndexBuf = nil
	self.edges2 = nil
end

-- if a vertex is near an edge (and no vertex) then split the edge and make another vertex next to it
-- TODO this is tempting me to store data like the OBJ file format, as unique positions and unique traits, but not as unique vertexes grouping all those traits...
function Mesh:splitVtxsTouchingEdges()
	timer('splitVtxsTouchingEdges', function()
		local edgeLenEpsilon = 1e-7		-- how long an edge has to be for considering it a legit edge - and considering it for splitting
		local edgeDistEpsilon = 1e-3	-- how close a vertex has to be to the edge
		local intervalEpsilon = 1e-3	-- how cloes to the interval endpoints a vertex has to be to consider splitting
::tryagain::
		for _,g in ipairs(self.groups) do
			for ti=g.triFirstIndex+g.triCount-1,g.triFirstIndex,-1 do
	--local debug = ({[6]=1,[8]=1,[9]=1,[17]=1})[ti]	-- these re the bad edges on target_basic bricks that need to be correctly split
	--local dprint = debug and print or function() end
				local tp = self.triIndexes.v + 3*ti
	--dprint('TRI', ti, 'with indexes', tp[0], tp[1], tp[2])
				for j=0,2 do
					local iv0 = tp[j]
					local iv1 = tp[(j+1)%3]
					local iv2 = tp[(j+2)%3]
	--dprint('EDGE', iv0, iv1)
					local v0 = ffi.new('MeshVertex_t', self.vtxs.v[iv0])
					local v1 = ffi.new('MeshVertex_t', self.vtxs.v[iv1])
					local edgeDir = v1.pos - v0.pos
					local edgePlanePos = .5 * (v1.pos + v0.pos)
					local edgeDirLen = edgeDir:norm()
	--dprint('...with len', edgeDirLen)
					if edgeDirLen > edgeLenEpsilon then
						edgeDir = edgeDir / edgeDirLen
						local edgePlane = plane3f():fromDirPt(edgeDir, edgePlanePos)
						local s0 = edgePlane:dist(v0.pos)	-- dist along the edge of v0
						local s1 = edgePlane:dist(v1.pos)	-- dist along the edge of v1
	--dprint('... and edge interval '..s0..' to '..s1)
						assert(s1 >= s0) -- because edgeDir points from v0 to v1
						for i=0,self.vtxs.size-1 do
							local vi = ffi.new('MeshVertex_t', self.vtxs.v[i])	-- copy so resizing the vec doesn't invalidate this
							if iv0 ~= i and iv1 ~= i and iv2 ~= i then
								local edgeDist = edgePlane:projectVec(vi.pos - edgePlanePos):norm()	-- how far from the edge is vi
								--print(edgeDist) --, math.abs(s - s0), math.abs(s - s1))
								-- if this vtx is close to the dge
	--dprint('testing against vertex', i, 'with edge dist', edgeDist)
								if edgeDist < edgeDistEpsilon then
									local s = edgePlane:dist(vi.pos)	-- dist along the edge of vi
	--dprint('vertex '..i..' has dist '..edgeDist..' and edge param '..s)
									-- and it is far from either endpoint of the edge
									if math.abs(s - s0) > intervalEpsilon
									and math.abs(s - s1) > intervalEpsilon
									and s0 < s and s < s1
									then
										-- then we have to split this triangle at this point in the interval
	--print("SPLITTING EDGE", v0.pos, v1.pos, 'at', vi.pos)
										local f = (s - s0) / (s1 - s0)
										local iv01 = self.vtxs.size
										local nvtx = self.vtxs:emplace_back()
										nvtx.pos = math.mix(v0.pos, v1.pos, f)
										nvtx.texcoord = math.mix(v0.texcoord, v1.texcoord, f)
										nvtx.normal = math.mix(v0.normal, v1.normal, f)

										-- [[ insertTri
										local nti = ti + 1
										tp[j] = iv0
										tp[(j+1)%3] = iv01
										tp[(j+2)%3] = iv2
										self.triIndexes:insert(self.triIndexes:begin() + 3*ti + 3, iv2)
										self.triIndexes:insert(self.triIndexes:begin() + 3*ti + 3, iv1)
										self.triIndexes:insert(self.triIndexes:begin() + 3*ti + 3, iv01)
	--dprint("mod'd", self.triIndexes.v[3*ti+0], self.triIndexes.v[3*ti+1], self.triIndexes.v[3*ti+2])
	--dprint('made', self.triIndexes.v[3*ti+3], self.triIndexes.v[3*ti+4], self.triIndexes.v[3*ti+5])
										self.tris:insert(ti+1, Triangle{
											index = nti+1,	-- 1-based
										})
										for _,g2 in ipairs(self.groups) do
											if nti <= g2.triFirstIndex then
												g2.triFirstIndex = g2.triFirstIndex + 1
											end
										end
										g.triCount = g.triCount + 1
										-- I can only split a triangl eonce, then  have to operate on the rest of the split tris
										goto tryagain
										--]]
									end
								end
							end
						end
					end
				end
			end
		end
	end)
	self:rebuildTris()
	self:mergeMatchingVertexes()	-- better to merge vtxs than remove empty tris cuz it will keep seams in models
	self:unloadGL()
end

-- 0-based, index-array so 3x from unique tri
function Mesh:triVtxs(ti)
	assert(ti >= 0 and ti + 3 <= self.triIndexes.size)
	local t = self.triIndexes.v + ti
	assert(t[0] >= 0 and t[0] < self.vtxs.size)
	assert(t[1] >= 0 and t[1] < self.vtxs.size)
	assert(t[2] >= 0 and t[2] < self.vtxs.size)
	return self.vtxs.v[t[0]],
			self.vtxs.v[t[1]],
			self.vtxs.v[t[2]]
end

-- 0-based, index-array so 3x from unique tri
function Mesh:triVtxPos(i)
	local a, b, c = self:triVtxs(i)
	return a.pos, b.pos, c.pos
end

function Mesh:removeEmptyTris()
print('removeEmptyTris from '..#self.tris)
	assert(#self.tris * 3 == self.triIndexes.size)
	for i=#self.tris,1,-1 do
		if self.tris[i].area < 1e-7 then
			self:removeTri(3*(i-1))
		end
	end
	assert(#self.tris * 3 == self.triIndexes.size)
print('removeEmptyTris to '..#self.tris)
end

-- rebuild .tris from .triIndexes
function Mesh:rebuildTris(from,to)
	if not from then
		from = 1
		to = self.triIndexes.size/3
	end
	for i,t in ipairs(self.tris) do
		assert(Triangle:isa(t))
	end
	for i=to+1,#self.tris do
		self.tris[i] = nil
	end
	for i=from,to do
		if not self.tris[i] then
			self.tris[i] = Triangle()
		end
		self.tris[i].index = i
		self.tris[i]:calcAux(self)
	end
	if #self.tris*3 ~= self.triIndexes.size then
		error("expected "..(#self.tris*3).." but found "..self.triIndexes.size)
	end
	for i,t in ipairs(self.tris) do
		assert(Triangle:isa(t))
	end
end

-- i know "TNB" is the traditional, cuz thats the order you calculate them in the Frenet frame
-- but if "normal" is the surface dir (which I'm making the 'Z' axis) so that u and v in 2D align with x and y in 3D ...
-- then it becomes tangent-(negative)binormal-normal
function Mesh:clearTriBasis(mesh)
	for i,t in ipairs(mesh.tris) do
		-- TODO unify this with .normal
		t.basis = nil
	end
end

-- generate tangent, binormal, normal
-- TODO merge normal with this
-- TODO TODO merge position with this so that 'pos normal texcoord-u texcoord-v' are a basis
-- pos should go first, not last.  just like time should go first, not last.
-- because ofc pos = eps rot around inf origin = integral of time-velocity = integral of exp map of Lorentz boost-generators (from acceleration)
function Mesh:generateTriBasis()
	--[[ make sure triangles have basis
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
	for i,t in ipairs(self.tris) do
		if not t.basis then
			local ti = 3*(i-1)
			assert(ti >= 0 and ti < self.triIndexes.size)
			local tp = self.triIndexes.v + ti
			assert(tp[0] >= 0 and tp[0] < self.vtxs.size)
			assert(tp[1] >= 0 and tp[1] < self.vtxs.size)
			assert(tp[2] >= 0 and tp[2] < self.vtxs.size)
			local va = self.vtxs.v[tp[0]]
			local vb = self.vtxs.v[tp[1]]
			local vc = self.vtxs.v[tp[2]]
			local dpos1 = vb.pos - va.pos
			local dpos2 = vc.pos - va.pos
			local dtc1 = vb.texcoord - va.texcoord	-- only considering 2D of it
			local dtc2 = vc.texcoord - va.texcoord
			-- dtc is matrix with columns of dtc[i]
			local dtc = matrix_ffi{
				{dtc1.x, dtc2.x},
				{dtc1.y, dtc2.y},
			}
			-- now 2x2 invert
			local dtcInv = dtc:inv()
			--assert((dtc * dtcInv - matrix_ffi{{1,0},{0,1}}):normSq() < 1e-7)

			-- get the cols
			local dtcInv1 = vec2f(dtcInv[1][1], dtcInv[2][1])
			local dtcInv2 = vec2f(dtcInv[1][2], dtcInv[2][2])

			local n = dpos1:cross(dpos2):normalize()

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
			-- [[ orthogonalize
			local ey = n:cross(ex):normalize()
			--]]
			t.basis = table{ex, ey, n}
--print(i, table.unpack(t.basis), n:dot(ex), n:dot(ey))
		end
	end
--[[
print('tri basis:')
for i,t in ipairs(self.tris) do
	print(t.basis:unpack())
end
--]]
end


--[[
fills the edges2 table
this holds a list of edges which are pairs of tris with shared vertexes.
each .edges2[] entry has exctly 2 tris in it.  tris can have up to 3 edges2[] entreis.
tris with edges that are borders of open meshes will no be in edges2. but tehy wil be in .edges[]
so adding them to edges2 is a big TODO

TODO
- get rid of .edges, and use this as the default edge structure instead

angleThresholdInDeg is used in calcEdges2 for 'isPlanar' calculations
and this can be relatively loose (5 deg or so) for allowing planar uv unwrapping around curves.

normEpsilon is for validating that the norm is nonzero
edgeDistEpsilon is for finding overlapping edges that don't share vertexes but we still want to uv-unwrap fold over.
edgeAngleThreshold is used for ensuring those edges are aligned, so this must be tighter than angleThresholdInDeg.
--]]
function Mesh:calcEdges2()
	local cosPlanarAngleThreshold = math.cos(math.rad(self.angleThresholdInDeg))

	local normEpsilon = 1e-7
	--local edgeDistEpsilon = 1e-7 -- ... is too strict for roof
	local edgeDistEpsilon = 1e-3
	-- TODO use getUniqueVtxs, instead of cycling through all?
	--  come to think of it, that's what getUniqueVtxs now does ...
	--local edgeAngleThreshold = math.rad(1e-1)
	--local cosEdgeAngleThreshold = math.cos(edgeAngleThreshold)

	assert(#self.tris*3 == self.triIndexes.size)

	-- new edge structure
	-- it only represents entire tri edges, like .edges (no subintervals)
	-- but it contains the normal, planar, etc data
	-- TODO this should be used to replace both
	-- hmm but .edges is based on all matching vtx pairs shared by edges (so unlimited tris per edge)
	-- while .edges2 is based on pairs of tri edges (so 2 tris at most per edge)
	self.edges2 = table()
	for _,t in ipairs(self.tris) do
		t.edges2 = table()
	end
--for _,t in ipairs(self.tris) do
--	print('n = '..t.normal)
--end
	local goodTris = 0
	local badTris = 0
	-- if I lower this to 1e-7 then it runs into cases of clipPlanes that don't separate their two triangle COMs ...
	local normalThreshold = 1e-7
	for ti1=#self.tris,2,-1 do
		local t1 = self.tris[ti1]
		if t1.normal:norm() > normalThreshold then
			local tp1 = self.triIndexes.v + 3 * (ti1 - 1)
			for j1=1,3 do
				-- t1's j1'th edge
				local v11 = self.vtxs.v[tp1[j1-1]].pos
				local v12 = self.vtxs.v[tp1[j1%3]].pos
	--print('tri', ti1, 'pos', j1, '=', v11)
				local edgeDir1 = v12 - v11
				local edgeDir1Norm = edgeDir1:norm()
				if edgeDir1Norm > normEpsilon then
					edgeDir1 = edgeDir1 / edgeDir1Norm
					for ti2=ti1-1,1,-1 do
						local t2 = self.tris[ti2]
						if t2.normal:norm() > normalThreshold then
							local tp2 = self.triIndexes.v + 3 * (ti2 - 1)
							for j2=1,3 do
								local v21 = self.vtxs.v[tp2[j2-1]].pos
								local v22 = self.vtxs.v[tp2[j2%3]].pos
								local edgeDir2 = v22 - v21
								local edgeDir2Norm = edgeDir2:norm()
								if edgeDir2Norm  > normEpsilon then
									edgeDir2 = edgeDir2 / edgeDir2Norm
									do --if math.abs(edgeDir1:dot(edgeDir2)) > cosEdgeAngleThreshold then
		--print('edges2 normals align:', ti1-1, j1-1, ti2-1, j2-1)
										-- normals align, calculate distance
										--local planePos = v11

										-- pick any point on line v1: v11 or v12
										-- or an average is best (when testing tri COM on either side tof the dividing plane)
										-- use the average of the two edges intersection with the plane, not just one edge arbitrarily
										local planePos = .25 * (v11 + v12 + v21 + v22)
										-- average the two edge-dirs
										-- make sure it points along 'edgeDir1'
										local edgeDir
										if edgeDir1:dot(edgeDir2) < 0 then
											edgeDir = edgeDir1 - edgeDir2
										else
											edgeDir = edgeDir1 + edgeDir2
										end
										local edgeDirLen = edgeDir:norm()
										if edgeDirLen > 1e-7 then
											edgeDir = edgeDir / edgeDirLen
											-- this is the edge projection plane, used for calculating distances to determine the interval of edge tri edge along this (the averaged edge)
											local plane = plane3f():fromDirPt(edgeDir, planePos)
											-- find ray from the v1 line to any line on v2
											-- project onto the plane normal
											-- calculate the distance of the points both projected onto the plane
											local dist = plane:projectVec(v21 - v11):norm()
											-- also calc dists of each vtx to one another
											-- only consider if both edge vtxs match
											-- no more subintervals
											local dist_11_21 = (v11 - v21):norm()
											local dist_12_22 = (v12 - v22):norm()
											local dist_12_21 = (v12 - v21):norm()
											local dist_11_22 = (v11 - v22):norm()
											-- TODO should I store them?
											if dist < edgeDistEpsilon
											and (
												(dist_11_21 < edgeDistEpsilon and dist_12_22 < edgeDistEpsilon)
												or (dist_12_21 < edgeDistEpsilon and dist_11_22 < edgeDistEpsilon)
											)
											then
												assert(ti2 < ti1)
												local s1, s2
												if dist_11_21 < edgeDistEpsilon
												and dist_12_22 < edgeDistEpsilon
												then
													s1 = plane:dist(.5 * (v11 + v21))
													s2 = plane:dist(.5 * (v12 + v22))
													-- TODO in this case we have a cw and ccw tri touching
													-- that's a bad thing, that means inside vs outside orientation is flipping
													-- ... and sure enough, there's a bad triangle in the mesh i'm given ...
													-- in fact this situation makes it tough to decide where exactly to put the clip plane ...
													-- maybe I should avoid it altogether?
													badTris = badTris + 1
												elseif dist_12_21 < edgeDistEpsilon
												and dist_11_22 < edgeDistEpsilon
												then
													goodTris = goodTris + 1
													s1 = plane:dist(.5 * (v11 + v22))
													s2 = plane:dist(.5 * (v12 + v21))

													-- in my loop ti2 < ti1, but i want it ordered lowest-first, so ... swap them
													local normAvg = (t1.normal + t2.normal):normalize()
													local clipPlane = plane3f():fromDirPt(normAvg:cross(edgeDir):normalize(), planePos)
													-- ok edgeDir is aligned with edgeDir1 ... = v12 - v11
													-- so clipPlane normal = normAvg cross edgeDir1 will point back to the t1 COM
													-- so clipPlane normal dot t1 normal > 0 for external edges, < 0 for internal edges
													-- TODO member functions for edge getters
													local e = Edge{
														tris = {t2, t1},
														triVtxIndexes = {j2, j1},
														interval = {s1, s2},
														plane = plane,
														planePos = planePos,
														normAvg = normAvg,
														clipPlane = clipPlane,
														-- specific to two-tri edges:
														dist = dist,
														isPlanar = t1.normal:dot(t2.normal) > cosPlanarAngleThreshold,
														isExtEdge = clipPlane.n:dot(t1.normal) > 0,
													}
													self.edges2:insert(e)
													t1.edges2:insert(e)
													t2.edges2:insert(e)
												else
													error"how did you get here?"
												end
											end
										end
									end
								end
							end
						end
					end
				end
			end
		end
	end
print("found "..goodTris.." good pairs and "..badTris.." bad pairs")

--[[
for _,e in ipairs(self.edges2) do
	print(
		'edges', self.tris:find(e.tris[1])-1, e.triVtxIndexes[1]-1,
		'and', self.tris:find(e.tris[2])-1, e.triVtxIndexes[2]-1,
		'align with dist', e.dist,
		'with projected intervals', table.concat(e.intervals[1], ', '),
		'and', table.concat(e.intervals[2], ', '))
end
print('found', #self.edges2, 'overlaps')
--]]
end

function Mesh:findEdges(getIndex)
	if not getIndex then getIndex = function(a) return a end end
	-- and just for kicks, track all edges
	if not self.edgeIndexBuf then
		self.edgeIndexBuf = vector('int32_t', 6 * #self.tris)
	end
	self.edgeIndexBuf:resize(0)

	assert(#self.tris*3 == self.triIndexes.size)
	for i=1,self.triIndexes.size/3 do
		local t = self.tris[i]
		if not Triangle:isa(t) then
			error("got a bad tri at "..i..": "..require 'ext.tolua'(t))
		end
	end
	timer('edges', function()
		self.edges = {}
		local function addEdge(a,b,t)
			if a > b then return addEdge(b,a,t) end
			self.edges[a] = self.edges[a] or {}
			local e = self.edges[a][b]
			if not e then
				-- new edge?  add it to the index buffer
				self.edgeIndexBuf:push_back(a-1)
				self.edgeIndexBuf:push_back(b-1)
				local va = self.vtxs.v[a-1]
				local vb = self.vtxs.v[b-1]
				local com = .5 * (va.pos + vb.pos)
				local edgeDir = (vb.pos - va.pos):normalize()
				local plane = plane3f():fromDirPt(edgeDir, com)
				e = Edge{
					[1] = a,
					[2] = b,
					tris = table(),
					length = (va.pos - vb.pos):norm(),
					-- because in tilemesh I'm mixing .edges and .edges2
					-- TODO build findBadEdges using .edges2 only
					-- but tht gets into 'isPlanar' and the angleThreshold being everywhere ....
					plane = plane,
					planePos = com,
					com = com,
					clipPlane = plane3f():fromDirPt(t.normal:cross(edgeDir):normalize(), com),
					normAvg = vec3f(t.normal),	-- where should normAvg point? planar?  tri normal?
					interval = {plane:dist(va.pos), plane:dist(vb.pos)},
				}
				self.edges[a][b] = e
			end
			e.tris:insert(t)
			t.edges:insert(e)
		end
		for i=0,self.triIndexes.size-1,3 do
			local tp = self.triIndexes.v + i
			local a = getIndex(tp[0])
			local b = getIndex(tp[1])
			local c = getIndex(tp[2])
			local ti = i/3+1
			local t = self.tris[ti]
			if not Triangle:isa(t) then
				error("got a bad tri at "..ti..": "..require 'ext.tolua'(t))
			end
			t.edges = table()
			addEdge(a+1, b+1, t)
			addEdge(a+1, c+1, t)
			addEdge(b+1, c+1, t)
		end
	end)
end

function Mesh:delaunayTriangulate()
	-- go through all triangles sharing edges ...
	-- flip any that don't fulfill the delaunay condition
	if not self.edges2 then
		self:calcEdges2()
	end
	local flipped
	repeat
		flipped = false
		for _,e in ipairs(self.edges2) do
			local t1, t2 = table.unpack(e.tris)
			local vi1, vi2 = table.unpack(e.triIndexes)
			local tp1 = self.triIndexes.v + 3*(t1.index-1)
			local tp2 = self.triIndexes.v + 3*(t2.index-1)
			-- v10 and v21 match, v11 and v20 match, v12 and v22 are opposites
			local i10 = tp1[vi1-1]
			local i11 = tp1[vi1%3]
			local i12 = tp1[(vi1+1)%3]
			local i20 = tp2[vi2-1]
			local i21 = tp2[vi2%3]
			local i22 = tp2[(vi2+1)%3]
			local v10 = self.vtxs.v[i10]
			local v11 = self.vtxs.v[i11]
			local v12 = self.vtxs.v[i12]
			local v20 = self.vtxs.v[i20]
			local v21 = self.vtxs.v[i21]
			local v22 = self.vtxs.v[i22]
			-- pick 1 of the 2 triangles as the 2D basis
			local a, c
			if i10 ~= i21 then
				assert((v10 - v21):norm() < 1e-5)
				c = .5 * (v10 + v21) - t1.uvorigin3D
			else
				c = v10
			end
			if i11 ~= i20 then
				assert((v11 - v20):norm() < 1e-5)
				a = .5 * (v11 + v20) - t1.uvorigin3D
			else
				a = v11
			end
			-- https://en.wikipedia.org/wiki/Delaunay_triangulation
			local b = v12 - t1.uvorigin3D
			local d = v22 - t1.uvorigin3D
			a = vec2f(a:dot(t1.basis[1]), a:dot(t1.basis[2]))
			b = vec2f(b:dot(t1.basis[1]), b:dot(t1.basis[2]))
			c = vec2f(c:dot(t1.basis[1]), c:dot(t1.basis[2]))
			d = vec2f(d:dot(t1.basis[1]), d:dot(t1.basis[2]))
			a = a - d
			b = b - d
			c = c - d
			local cond = matrix_ffi{
				{a.x, a.y, a:normSq()},
				{b.x, b.y, b:normSq()},
				{c.x, c.y, c:normSq()},
			}:det()
			if cond <= 0 then
				flipped = true
				-- flip ...
				tp1[vi1-1] = i12
				tp1[vi1%3] = i22
				tp1[(vi1+1)%3] = i11	-- or i20
				tp2[vi2-1] = i22
				tp2[vi2%3] = i12
				tp2[(vi2+1)%3] = i10	-- or i21
				if i11 ~= i20 then
					self.vtxs.v[i11].pos = .5 * (self.vtxs.v[i11].pos + self.vtxs.v[i20].pos)
				end
				if i10 ~= i21 then
					self.vtxs.v[i10].pos = .5 * (self.vtxs.v[i10].pos + self.vtxs.v[i21].pos)
				end
				-- now recalculate the edge variables
				-- and recalculate the triangle variables
				error"TODO"
			end
		end
	until not flipped
end

--[[
calculates .triGroups based on neighboring tris' edges' .isPlanar
calculates .triGroupForTri to map from each tri to .triGroups
(this means if you regenerate all triangles it'll lose assocation ...
 ... unless I change .triGroupForTri to map from index to group)
uses .edges2 and :findBadEdges (which uses .edges)
--]]
function Mesh:calcTriSurfaceGroups()
	if not self.edges2 then
		self:calcEdges2()
	end

	-- [[ group all tris based on boundaries of angles
	local triGroups = table(self.tris):mapi(function(t)
		return {
			tris = table{t},
		}
	end)
	do
		local numMerges = 0
		local found
		repeat
			found = false
			for _,e in ipairs(self.edges2) do
				if e.isPlanar then
					local i1, g1 = triGroups:find(nil, function(g) return g.tris:find(e.tris[1]) end)
					local i2, g2 = triGroups:find(nil, function(g) return g.tris:find(e.tris[2]) end)
					if i1 ~= i2 then
						triGroups[i1].tris:append(triGroups[i2].tris)
						triGroups:remove(i2)
						found = true
						numMerges = numMerges + 1
						break
					end
				end
			end
		until not found
print('performed', numMerges, 'merges')
	end
-- [[
print('found '..#triGroups..' groups of triangles')
for _,g in ipairs(triGroups) do
	io.write('...group of '..#g.tris..' :')
	for _,t in ipairs(g.tris) do
		io.write(' ', t.index)
	end
	print()
end
--]]
	self.triGroups = triGroups
	-- make a mapping back from triangles to their groups
	local triGroupForTri = self.tris:mapi(function(t)
		for _,g in ipairs(triGroups) do
			if g.tris:find(t) then return g, t end
		end
		error("shouldn't get here")
	end)
	-- gather all edges to this group
	-- TODO also add 'findHoles' edges to the self as planes ... perp to the surface i guess?
	for _,g in ipairs(triGroups) do
		g.borderEdges = table()
	end

	assert(#self.tris*3 == self.triIndexes.size)
	for ti=0,#self.tris-1 do
		local t = self.tris[ti+1]
		local tp = self.triIndexes.v + 3*ti
		local g = assert(triGroupForTri[t])
		for j=0,2 do
			-- find any overlapping edges at this vtx of this tri
			local foundAnyEdge
			local foundClipEdge
			for _,e in ipairs(self.edges2) do
				for k=1,2 do
					if e.tris[k] == t and e.triVtxIndexes[k] == j+1 then
						foundAnyEdge = true
						local ot = e.tris[3-k]
						local g2 = assert(triGroupForTri[ot])
						if g ~= g2 then
							local tside = e.clipPlane:test(t.com)
							e.isGroupBorderEdge = true
							-- TODO I could just store the edge and a flag for whether to flip the clip plane ...
							g.borderEdges:insert{edge=e, clipPlane=tside and e.clipPlane or -e.clipPlane}
							foundClipEdge = true
							break
						end
					end
				end
				-- allow multiple clipPlane entries for the same edge?
				--if foundClipEdge then break end
			end
			-- [=[ if we didn't find any adjacent triangles to this tri on this edge - whatsoever
			-- - then we consider this a clip plane also
			-- (this is what the findBadEdges was looking for, but it only works if our mesh is simply closed connected)
			if not foundAnyEdge then
				local v1 = self.vtxs.v[tp[j]].pos
				local v2 = self.vtxs.v[tp[(j+1)%3]].pos
				-- hmm...
				v1, v2 = v2, v1
				local com = .5 * (v1 + v2)
				-- why did I have to make the edgedir negative for the clipping to work?
				local edgeDir = v2 - v1
				local edgeDirLen = edgeDir:norm()
				if edgeDirLen > 1e-7 then
					edgeDir = edgeDir / edgeDirLen
					local planePos = v1
					local plane = plane3f():fromDirPt(edgeDir, planePos)
					local s1 = plane:dist(v1)
					local s2 = plane:dist(v2)
					assert(s1 <= s2)
print('interval', s1, s2)
					local clipNormal = t.normal:cross(edgeDir):normalize()
					local clipPlane = plane3f():fromDirPt(clipNormal, com)
print('clipPlane', clipPlane)
					local e = Edge{
						tris = {t},
						triVtxIndexes = {j+1},	-- TODO make this 0-based, here and in calcEdges2 ...
						interval = {s1, s2},
						plane = plane,
						planePos = planePos,
						clipPlane = clipPlane,
						normAvg = vec3f(t.normal),
						com = com,
						isGroupBorderEdge = true,
					}
					local tside = e.clipPlane:test(t.com)
					g.borderEdges:insert{edge=e, clipPlane=tside and e.clipPlane or -e.clipPlane}
					-- keep here so i can search these as well as t.edges2 when looking for adjacent edges
					t.borderEdges = t.borderEdges or table()
					t.borderEdges:insert(e)
				end
			end
			--]=]
		end
	end

	self.triGroupForTri = triGroupForTri
end

-- calculate the edge clip-plane-groups
function Mesh:calcTriEdgeGroups()
	if not self.triGroups then
		self:calcTriSurfaceGroups()
	end
	-- build edge groups as well
	-- ... and insert them into the 'triGroups' as well ...
	-- ... I should really put this in its own function off of 'calcTriSurfaceGroups'
	-- ... and flag some groups as surface, others as edge ...
	-- ... or just put them in a new data structure?
	--local vtxMergeThreshold = 1e-4	-- causes neighboring edges at target_complex-roof to merge
	local vtxMergeThreshold = 1e-5
	local uniquevs, indexToUniqueV = self:getUniqueVtxs(vtxMergeThreshold)
	local function uniquevtx(vi) return uniquevs[indexToUniqueV[vi]] end

	--[[
	new algorithm
	1) go through all border edges (of tri groups and of open meshes)
	2) flag all their vertexes at each endpoint (via uniquevtx)
		2.a) don't flag the ones that are in straight lines.  or within tolerance for curved corners.
	3) cycle through all those vertexes
	 ... we have to assume the touching vtxs are a 2D surface.  though there are T's in our shitty quality meshes. hmm.
	4) between each two edges around the vtx, insert a new fake-edge.  give a coyp of the fake-clip-edge to each edge's clipGroup, pointed back at that edge's COM
	--]]

	self.edgeClipGroups = table() 	-- key is the edge
	local function makeEdgeClipGroups(e)
--print('makeEdgeClipGroup', e)
		local _, eg = self.edgeClipGroups:find(nil, function(eg)
			return eg.srcEdges:find(nil, function(es)
				return es.edge == e
			end)
		end)
		if not eg then
			eg = {
				-- this is the real edges that map to this edge-clip-group
				-- it can be from t.edges2 or from t.borderEdges
				srcEdges = table(),
				-- this is the fake-edges to-be-used for clipping
				borderEdges = table(),
			}
			self.edgeClipGroups:insert(eg)
		end
		-- insertUnique ...
		if not eg.srcEdges:find(nil, function(es) return es.edge == e end) then
			assert(#eg.srcEdges == 0)	-- hmmmmmmm how to insert and maintain srcEdges' order ...
			eg.srcEdges:insert{
				edge = e,
				intervalIndex = 1,
			}
		end
		return eg
	end

	-- maps from (unique) vtx index to list of outer edges
	local outerEdgesForVtx = {}

	for _,tg in ipairs(self.triGroups) do
		for _,info in ipairs(tg.borderEdges) do
			local e = info.edge
print()
print('starting on edge pos', e.planePos, 'normal', e.plane.n)

			-- now clip against endpoint edges
			-- for each vtx of the edge ...
			--  for all other tris on vtx ...
			--   (if the tri and the vertex don't touch another triGroup-brorder-edge then keep going until we run out of tris or reach this first edge again.)
			--   make clip plane at the edge and use it in our clip polytope
			-- TODO might need the uniquevs used to create edges2 here ...
			-- TODO TODO maybe we should be using it to make edges2 in the first place ...
			local eg = makeEdgeClipGroups(e)

			local vi1 = self.triIndexes.v[3*(e.tris[1].index-1) + e.triVtxIndexes[1]-1]
			local vi2 = self.triIndexes.v[3*(e.tris[1].index-1) + e.triVtxIndexes[1]%3]
			local normAvg = table.mapi(e.tris, function(t) return t.normal end):sum():normalize()
			vi1 = uniquevtx(vi1)
			vi2 = uniquevtx(vi2)

			-- [[ based on vtx positions
			local com = .5 * (self.vtxs.v[vi1].pos + self.vtxs.v[vi2].pos)
			--]]
			--[[ based on edge intervals
			local com = e.com
--print('FINDING PLANES FOR EDGE WITH COM', com)
			--]]

			-- find the next tri with vtx at edge 'vi' and touches edge 'prevt'
			-- stop if you come back to the start edge  'e'
			-- stop if you cover the same triangle twice
			-- stop if you find a triGroup boundary edge
			--  if you do find a triGroup boundary edge then
			--   create a clip plane midway between the start edge 'e' and the triGroup boundary edge
			local trisHaveBeenTested
			local function propagateEdge(vi, edgeDir, preve, tristack, totalAngle)

-- convention : assert that our edgeDir points towards the COM
local edgeDot = (com - self.vtxs.v[vi].pos):dot(edgeDir)
assert(edgeDot >= 0)
--if edgeDot < 0 then edgeDir = -edgeDir end

print('#tristack', #tristack, 'totalAngle', math.deg(totalAngle), 'edgeDot', edgeDot)

				vi = uniquevtx(vi)
				-- for all tris touching the previous edge ...
				-- (they also have this vtx in common)
				for _,t in ipairs(preve.tris) do
					if not trisHaveBeenTested:find(t) then
						trisHaveBeenTested:insert(t)
						local tp = self.triIndexes.v + 3*(t.index-1)

						local nexttristack = table(tristack)
						nexttristack:insert(t)

						-- find their edges
						-- have to look in this tri's edges
						-- but all those trigroup borderEdges were added extra, so have to check those too ...
						for _,e2 in ipairs(table():append(t.edges2, t.borderEdges)) do
							if e2 ~= preve then
								local t2 = e2.tris[1]
								local tp2 = self.triIndexes.v + 3*(t2.index-1)
								for j2=1,2 do
									local vo = uniquevtx(tp2[(e2.triVtxIndexes[1]-1+j2-1)%3])
									-- ... that also share this vertex in common ...
									if vo == vi then
										-- get the angle that this triangle makes with vi
										local k = range(3):find(nil, function(k)
											return uniquevtx(tp[k-1]) == vo
										end)
										--assert(k)
										-- this assert fails for non-manifold meshes.
										if not k then
											print("!!! WARNING !!! couldn't find vertex in triangle associated with edge.  is the mesh non-manifold?")
										else
											k=k-1	-- from index to offset
											assert(k >= 0 and k < 3)
											local angle = math.acos(math.clamp(
												(self.vtxs.v[tp[(k+1)%3]].pos - self.vtxs.v[tp[k]].pos)
												:normalize():dot(
													(self.vtxs.v[tp[(k+2)%3]].pos - self.vtxs.v[tp[k]].pos):normalize()
												),
												-1, 1))
print('adding angle', math.deg(angle))
											local nextTotalAngle = totalAngle + angle

											if not e2.isGroupBorderEdge then
												-- ... if it's not a triGroup boundary then keep looking
												propagateEdge(vi, edgeDir, e2, nexttristack, nextTotalAngle)
											else

												-- ok here ... we're considering between edges 'e' and 'e2' ...
												-- because I am following triangles to get here, I can account for T's in meshes (if the mesh has errors in it)
												-- but because I'm following triangles, I can't loop around the whole vertex when we're at an open mesh's border.
												-- This skips exterior angles.
												-- I could account for exterior angles if instead of doing this I simply found all vtxs used by meshes and rotated around them, inserting fake-planes between.
												--  but doing that would skip T's in meshes provided by incompotent modellers / CAD users.
												-- so how do I combine the two algorithms?
												-- by storing all outside-edges that I hit on a single vertex (there should always only be 0 or 2)
												-- and then if we found 2, put a fake-edge between them.
												-- how to find an outside edge?
												-- it'll have here 'isExtEdge == nil'
												if e2.isExtEdge == nil then
													outerEdgesForVtx[vi] = outerEdgesForVtx[vi] or {
														outerEdges = table(),
														normAvg = normAvg,
													}
													outerEdgesForVtx[vi].outerEdges:insertUnique(e2)
												end

print('ending on edge pos', e2.planePos, 'normal', e2.plane.n)
print('adding fake edge with tri stack', #nexttristack, 'total angle', math.deg(nextTotalAngle))

												-- ... if it's a triGroup boundary  ....
												-- ... then use it as a clip plane
												-- now edgeDir is the vector along 'e' that points from 'vi' to its opposite
												local edgeDir2 = j2 == 1 and -e2.plane.n or e2.plane.n
												-- edgeDir2 is the vector along 'e2' that points from 'vo' to the opposite
												--if edgeDir2:dot(edgeDir) < 0 then print('edgeDir2:dot(edgeDir) < 0') end
												--if edgeDir2:dot(edgeDir) < 0 then edgeDir2 = -edgeDir2 end

												-- ok the triangle stack ...
												-- the fake-edge needs to be pointed along the average angle *within the plane* of the triangles
												-- so if the two edges' common plane makes a >180 degree, we want the fake-edge to point along that >180 degree
												-- so just testing dot product isn't enough.
local tristacknormaldots = require 'matrix'{#nexttristack,#nexttristack}:lambda(function(i,j)
	local d = nexttristack[i].normal:dot(nexttristack[j].normal)
	-- tris should all be aligned ...
	--  ...for the tri gruops to have been created in the first place ...
	-- this assert fails for curved walls:
	--assert(d > 1 - 1e-3)
	return d
end)
print('tri stack angles:')
print(tristacknormaldots)
												-- get the surface normal
												--local triavgnormal = nexttristack:mapi(function(t) return t.normal end):sum():normalize()
												-- get a basis from the normal.  you can use edgeDir as one of the basis
												--local e1 = edgeDir:normalize()
												--local e2 = triavgnormal:cross(edgeDir):normalize()
												--local e3 = vec3f(triavgnormal)
												-- alright if edgeDir is e1 then edgeDir has coordinates [1,0] in our {e1,e2} basis
												-- then edgeDir2 has coordinates ...
												--local edgeDir2SurfBasisCoords = vec2f(e1:dot(edgeDir2), e2:dot(edgeDir2))
												-- but still how do we know which dir the triangles are going around the edge?
												-- the tri.avg.normal ... ?
												-- how about whether we had to flip edgeDir2 ...
												-- how about ... depending on t2's rotation?

												local edgeDirAvg = (edgeDir + edgeDir2):normalize()
												-- ok if the total angle made in this plane is > 180 then we will have to flip edgeDirAvg
												if nextTotalAngle > math.pi then edgeDirAvg = -edgeDirAvg end

												-- how to form a right angle to point back at 'e'? triple cross product?
												local edgePlaneNormal = edgeDir2:cross(edgeDir):normalize()
												local clipPlaneNormal = edgeDirAvg:cross(edgePlaneNormal)
												local clipPlaneNormalLen = clipPlaneNormal:norm()
												if clipPlaneNormalLen > 1e-7 then
													clipPlaneNormal = clipPlaneNormal / clipPlaneNormalLen
													local clipPlane = plane3f():fromDirPt(clipPlaneNormal, self.vtxs.v[vi].pos)
													-- then add a clip plane between these two edges,
													-- make sure it's pointing at the first edge.
print('...adding clip plane '..clipPlane)
													-- TODO the edge of the clip plane isn't the edge we got it from
													-- it's a new edge halfway
													-- needs to abstract the edge's getters for vertex endpoints
													-- since that's what clip to group function uses
													local fakeEdge = Edge()
													-- planePos should lie at the edge endpoint
													-- and plane.n should point down the edge
													fakeEdge.planePos = vec3f(self.vtxs.v[vi].pos)
													fakeEdge.plane = plane3f():fromDirPt(edgeDirAvg, fakeEdge.planePos)
													-- TODO NOTICE BIG WARNING
													-- I was setting this to {0,1} ...
													-- .. but that would make the clip group algo sometimes toss some edges ...
													-- so ... needs t be smaller ..
													-- but since this is a fake edge -- how small sohuld it be?
													fakeEdge.interval = {0, .1}
													--[[ basis for orientation of tiles placed
													fakeEdge.basis = {e1, e2, e3}
													--]]  -- why is this messed up?
													-- [[ instead, this was there before ...
													-- TODO this basis needs to have its 'fakeEdge.plane.n' vector pointing outwards
													-- also .. i don't think I'm using this atm
													fakeEdge.basis = {
														normAvg:cross(fakeEdge.plane.n):normalize(),
														normAvg,
														fakeEdge.plane.n,
													}
													--]]
													-- for debugging ... only?
													-- for consistency this should be calculated by the interval midpoint ...
													fakeEdge.com = vec3f(com)
													local tside = clipPlane:test(com)
													eg.borderEdges:insert{edge=fakeEdge, clipPlane=tside and clipPlane or -clipPlane}
												end
											end
										end
									end
								end
							end
						end
					end
				end
			end

			for _,info in ipairs{
				-- vi1 is the 'from' in the edge ray from->to
				-- so e.plane.n points into vi1
				-- so for vi1 use the edgeDir pointing away from vi1 i.e. negative
				-- so that for finding clip planes around the edge, vi1 can be the origin
				{vi=vi1, plane=-e.plane.n},
				-- and reverse for vi2
				-- reset this between each test
				{vi=vi2, plane=e.plane.n},
			} do

				local vi = info.vi
				trisHaveBeenTested = table()
				propagateEdge(vi, info.plane, e, table(), 0)
			end
		end
	end

	for vi,info in pairs(outerEdgesForVtx) do
		local outerEdges = info.outerEdges
		local normAvg = info.normAvg
		-- hmm, some have 1
		-- I.g. those are outer-edges with no inner-edges connecting to them, esp if they are sequences-of-edges
		--assert(#outerEdges >= 0 and #outerEdges <= 2, "got bad # outer edges: "..#outerEdges)
print('at vertex '..self.vtxs.v[vi].pos..' #outerEdges', #outerEdges)
-- [=[
		if #outerEdges == 2 then
			local edge1 = outerEdges[1]
			local edge2 = outerEdges[2]
			local edgeDir = edge1.plane.n
			local edgeDir2 = edge2.plane.n
			if (self.vtxs.v[vi].pos - edge1.com):dot(edgeDir) < 0 then edgeDir = -edgeDir end
			if (self.vtxs.v[vi].pos - edge2.com):dot(edgeDir2) < 0 then edgeDir2 = -edgeDir2 end
			-- then insert a fake-edge averaged between them.  using angle bisection in case the angle between them is >180
			-- but rlly how cna you tell?  seems you'll have to use more info, like which side their triangles are on ...
			local edgePlaneNormal = edgeDir2:cross(edgeDir):normalize()
			local edgeDirAvg = (edgeDir + edgeDir2):normalize()
			local clipPlaneNormal = edgeDirAvg:cross(edgePlaneNormal)
			local clipPlaneNormalLen = clipPlaneNormal:norm()
			if clipPlaneNormalLen  > 1e-7 then
				clipPlaneNormal = clipPlaneNormal / clipPlaneNormalLen
				local clipPlane = plane3f():fromDirPt(clipPlaneNormal, self.vtxs.v[vi].pos)

				local planePos = vec3f(self.vtxs.v[vi].pos)
				local plane = plane3f():fromDirPt(edgeDirAvg, planePos)

				local fakeEdge1 = Edge{
					planePos = planePos,
					plane = plane,
					interval = {0, 1},
					basis = {
						normAvg:cross(plane.n):normalize(),
						normAvg,
						plane.n,
					},
					com = edge1.com,
				}
				local tside = clipPlane:test(edge1.com)
				makeEdgeClipGroups(edge1).borderEdges:insert{edge=fakeEdge1, clipPlane=tside and clipPlane or -clipPlane}

				local fakeEdge2 = Edge{
					planePos = planePos,
					plane = plane,
					interval = {0, 1},
					basis = {
						normAvg:cross(plane.n):normalize(),
						normAvg,
						plane.n,
					},
					com = edge2.com,
				}
				clipPlane = -clipPlane
				local tside = clipPlane:test(edge2.com)
				makeEdgeClipGroups(edge2).borderEdges:insert{edge=fakeEdge2, clipPlane=tside and clipPlane or -clipPlane}

			end
		end
--]=]
	end

print'before merging edge groups:'
	for i,eg in ipairs(self.edgeClipGroups) do
		print('edge group',i,'#srcEdges', #eg.srcEdges)
	end
	local totalSrcEdgesBeforeMerging = self.edgeClipGroups:mapi(function(eg) return #eg.srcEdges end):sum()
print('total before', totalSrcEdgesBeforeMerging)

-- [=[
	-- now while we're here, do like with tri groups, and merge the ones that are within angle epsilon
	local cosAngleThreshold = math.cos(math.rad(self.angleThresholdInDeg))
	local distEps = 1e-5
	local found
	repeat
		found = false
		for i1=1,#self.edgeClipGroups-1 do
			local eg1 = self.edgeClipGroups[i1]

			-- ok ... convention ... ?
			-- srcEdges needs to keep these in-order
			-- srcEdges[i].intervalIndex will be the index (1 or 2) into srcEdges[i].edge.interval that gives us the param of the start of the edge
			-- so 3-intervalIndex will give us the index of the param of the end of the edge
			local es11 = eg1.srcEdges[1] 	-- src edge start info
			local e11 = es11.edge
			local v11 = e11.planePos + e11.plane.n * e11.interval[es11.intervalIndex]
			local es12 = eg1.srcEdges:last()
			local e12 = es12.edge
			local v12 = e12.planePos + e12.plane.n * e12.interval[3 - es12.intervalIndex]

			for i2=i1+1,#self.edgeClipGroups do
				local eg2 = self.edgeClipGroups[i2]
				-- if any in eg1 and eg2 are within threshold
				-- and their points align
				-- and they only have 2 edge neighbors?
				-- i think so yeah.
				-- then merge the two groups

				-- shouldn't contain eachothers edges ...
				for _,e1 in ipairs(eg1.srcEdges) do
					assert(not eg2.srcEdges:find(nil, function(es)
						return es.edge == e1.edge
					end))
				end

				local es21 = eg2.srcEdges[1]
				local e21 = es21.edge
				local v21 = e21.planePos + e21.plane.n * e21.interval[es21.intervalIndex]
				local es22 = eg2.srcEdges:last()
				local e22 = es22.edge
				local v22 = e22.planePos + e22.plane.n * e22.interval[3 - es22.intervalIndex]

				local function reverseEdgeGroupSrcEdges(eg)
					-- flip eg srcEdges
					eg.srcEdges = eg.srcEdges:reverse()
					for _,es in ipairs(eg.srcEdges) do
						es.intervalIndex = 3 - es.intervalIndex
					end
				end

				local vavg
				if (v11 - v21):norm() < distEps
				and math.abs(e11.plane.n:dot(e21.plane.n)) > cosAngleThreshold
				then
					-- reverse eg1 so its end matches eg2's start
					reverseEdgeGroupSrcEdges(eg1)
					vavg = .5 * (v11 + v21)
					found = true
				elseif (v11 - v22):norm() < distEps
				and math.abs(e11.plane.n:dot(e22.plane.n)) > cosAngleThreshold
				then
					-- we can either flip both or we can swap eg1 and eg2 ...
					reverseEdgeGroupSrcEdges(eg1)
					reverseEdgeGroupSrcEdges(eg2)
					vavg = .5 * (v11 + v22)
					found = true
				elseif (v12 - v21):norm() < distEps
				and math.abs(e12.plane.n:dot(e21.plane.n)) > cosAngleThreshold
				then
					-- already good
					vavg = .5 * (v12 + v21)
					found = true
				elseif (v12 - v22):norm() < distEps
				and math.abs(e12.plane.n:dot(e22.plane.n)) > cosAngleThreshold
				then
					-- reverse eg2 so its start matches eg1's end
					reverseEdgeGroupSrcEdges(eg2)
					vavg = .5 * (v12 + v22)
					found = true
				end
				if found then
					-- merge eg1 and eg2 srcEdges. maintain them in-order
					eg1.srcEdges:append(eg2.srcEdges)
					-- merge eg1 and eg2 borderEdges
					eg1.borderEdges:append(eg2.borderEdges)
					-- and while we're here, remove any fake edges whose planePos's are at the same location
					for j=#eg1.borderEdges,1,-1 do
						if (eg1.borderEdges[j].edge.planePos - vavg):norm() < distEps then
							eg1.borderEdges:remove(j)
						end
					end
					-- and remove eg2 from edgeClipGroups
					self.edgeClipGroups:remove(i2)
					break
				end
			end
			if found then break end
		end
	until not found

	-- by here, post-merge ,our groups should all either be e.isExtEdge==nil or e.isExtEdge == not nil (true or false)
	-- hmm maybe I can even assert the true is consistent or false is consistent
	for _,eg in ipairs(self.edgeClipGroups) do
		local es1 = eg.srcEdges[1]
		for i=2,#eg.srcEdges do
			local es = eg.srcEdges[i]
			if es.edge.isExtEdge ~= es1.edge.isExtEdge then
				print('!!! WARNING !!! edge group has mixed isExtEdge members.  is the mesh non-manifold?')
			end
		end
	end

	print'after merging edge groups:'
	for i,eg in ipairs(self.edgeClipGroups) do
		print('edge group',i,'#srcEdges', #eg.srcEdges)
	end
	local totalSrcEdgesAfterMerging = self.edgeClipGroups:mapi(function(eg) return #eg.srcEdges end):sum()
print('total after', totalSrcEdgesAfterMerging)
	assert(totalSrcEdgesBeforeMerging == totalSrcEdgesAfterMerging)

--]=]
	-- ok now if any edge's groups of fake clipplanes only has a single clipplane per vertex,
	-- ... due to it being the border of an open surface where an interior edge meets ..
	-- ... then we need to add an extra clip plane by extending that
	-- in fact
	-- once we're done placing fake-edges ...
	-- for each vtx
	--   make sure the real-edges and fake-edges intersperse
	--   if any fake-edges are missing then insert them.
--print('for edge+vtx made '..(#eg.borderEdges - oldNumBorderEdges)..' clip edges')
	-- THIS WOULD WORK PERFECTLY IF WE COULD GUARANTEE OUR MESH WAS A PROPR N-MANIFOLD EVERYWHERE ... WITH NO DANGLING EDGES OR TRIANGLES
	-- but because we could have weird T intersections ... AND WE DO ... on that stupid target_basic-bricks model which is screwed up ... this won't work there.
end

-- clip the current mesh to the specified trigroup
function Mesh:clipToClipGroup(g)
	--[[
	ARBITRARY POLYHEDRA/POLYTOPE CLIPPING FUNCTION

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
		assert(#edgeInfos > 0)
		local info = edgeInfos:remove()
		-- clone and clip against the -plane -> backMesh
		local backMesh = clipped:clone()
		local backModified = backMesh:clip(-info.clipPlane)
		if #backMesh.tris == 0 then
			backMesh = nil
		end
		-- clone and clip the plane -> frontMesh
		local frontMesh = clipped:clone()
		local frontModified = frontMesh:clip(info.clipPlane)
		if #frontMesh.tris == 0 then
			frontMesh = nil
		end
		-- if that plane was the list on our list ...
		if #edgeInfos == 0 then
			-- ... then toss the backMesh because it is outside
			-- and just return frontMesh (if we have it)
			if backMesh then
				anythingRemoved = true
			end
			backMesh = nil
		else
			local edgesFront, edgesBack = clipEdgesAgainstEdges(edgeInfos, info.clipPlane)
			if #edgesBack == 0 then
				-- backMesh is fully outside the poly -- toss it
				if backMesh then
					anythingRemoved = true
				end
				backMesh = nil
			else
				if backMesh then
					backMesh = clipMeshAgainstEdges(edgesBack, backMesh)
				end
			end
			if #edgesFront > 0 then
				if frontMesh then
					frontMesh = clipMeshAgainstEdges(edgesFront, frontMesh)
				end
			else
				-- frontMesh is now fully inside the poly, so keep it (if we have it)
			end
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
	local clipped
	if #g.borderEdges > 0 then
		clipped = clipMeshAgainstEdges(table(g.borderEdges), self)
	else
		clipped = self:clone()
	end
	-- if clipped is nil then everything was removed

	return clipped, anythingRemoved
end



--[[
calculate and store COMs
TODO store these?  or only calculate upon demand?
this will have to be recalculated every time the mesh changes
a prereq for calcCOM1 is findEdges()
--]]
function Mesh:calcCOMs()
	timer('com0', function()
		self.com0 = self:calcCOM0()
	end)
	print('com0 = '..self.com0)
	timer('com1', function()
		self.com1 = self:calcCOM1()
	end)
	print('com1 = '..self.com1)
	timer('com2', function()
		self.com2 = self:calcCOM2()
	end)
	print('com2 = '..self.com2)
	timer('com3', function()
		self.com3 = self:calcCOM3()
	end)
	print('com3 = '..self.com3)
	-- can only do this with com2 and com3 since they use tris, which are stored per-material
	-- ig i could with edges and vertexes too if I flag them per-material
	timer('group com2/3', function()
		for _,g in ipairs(self.groups) do
			g.com2 = self:calcCOM2(g.name)
			g.com3 = self:calcCOM3(g.name)
		end
	end)
end

-- replace all instances of one vertex index with another
function Mesh:replaceVertex(from,to)
--print('replacing vertex ' ..from..' with '..to)
	assert(from > to)
	assert(from >= 0 and from < self.vtxs.size)
	assert(to >= 0 and to < self.vtxs.size)
	-- replace in .tris
	for j=self.triIndexes.size-3,0,-3 do
		local tp = self.triIndexes.v + j
		for i=0,2 do
			if tp[i] == from then tp[i] = to end
		end
	end
end

function Mesh:removeDegenerateTriangles()
	for i=self.triIndexes.size-3,0,-3 do
		local tp = self.triIndexes.v + i
		for j=2,1,-1 do
			if tp[j] == tp[j-1] then
--print('removing degenerate tri '..i..' with duplicate vertices')
				self:removeTri(i)
				break
			end
		end
	end
end

-- index is 0-based in increments of 3
function Mesh:removeTri(i)
	if #self.tris*3 ~= self.triIndexes.size then
		error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
	end
	self.triIndexes:erase(self.triIndexes.v + i, self.triIndexes.v + i + 3)
	for _,g in ipairs(self.groups) do
		if i < 3*g.triFirstIndex then
			g.triFirstIndex = g.triFirstIndex - 1
		elseif i >= 3*g.triFirstIndex and i < 3*(g.triFirstIndex + g.triCount) then
			g.triCount = g.triCount - 1
		end
	end
	self.tris:remove(i/3+1)
	for j=i/3+1,#self.tris do
		self.tris[j].index = j
	end
end

-- remove all instances of a veretx index
-- remove the vertex from the elf.vs[] list
-- decrement the indexes greater
function Mesh:removeVertex(vi)
	assert(vi >= 0 and vi < self.vtxs.size)
	self.vtxs:erase(self.vtxs.v + vi, self.vtxs.v + vi + 1)
	-- remove in .tris
	-- if you did :replaceVertex and :removeDegenerateFaces first then the rest shouldn't be necessary at all (except for error checking)
	-- if you just straight up remove a vertex then the tris and faces might go out of sync
	for j=self.triIndexes.size-3,0,-3 do
		local tp = self.triIndexes.v + j
		for i=0,2 do
			if tp[i] == vi then
				--error("found a to-be-removed vertex index in a tri.  you should merge it first, or delete tris containing it first.")
				self:removeTri(j)
				break
			elseif tp[i] > vi then
				tp[i] = tp[i] - 1
			end
		end
	end
end

-- TODO just use a single dense tri array
-- don't use indexes at all

--[[
1) replace the 'from' with the 'to'
2) remove any degenerate triangles/faces
3) remove the to vertex from the list

-- TODO same for .vts and .vns ?
--]]
function Mesh:mergeVertex(from,to)
	assert(from > to)
	self:replaceVertex(from,to)
	self:removeDegenerateTriangles()
	self:removeVertex(from)
end

-- hmm, nobody is calling this anymore....
function Mesh:removeUnusedVtxs()
	local usedVs = {}
	timer('finding used vertexes', function()
		for i=0,self.triIndexes.size-1 do
			usedVs[self.triIndexes.v[i]] = true
		end
	end)
	timer('removing unused vertexes', function()
print('before removing, #vs', self.vtxs.size)
		for i=self.vtxs.size-1,0,-1 do
			if not usedVs[i] then
				self:removeVertex(i)
			end
		end
print('after removing, #vs', self.vtxs.size)
	end)
end

--[[ fixme
--nti is the 0-based triangle index
function Mesh:insertTri(a,b,c,nti)
	self.triIndexes:insert(self.triIndexes:begin() + 3*nti, a)
	self.triIndexes:insert(self.triIndexes:begin() + 3*nti+1, b)
	self.triIndexes:insert(self.triIndexes:begin() + 3*nti+2, c)
	self.tris:insert(nti, Triangle{
		index = nti+1,	-- +1 cuz its' 1-based
	})
	for _,g in ipairs(self.groups) do
		if nti <= g.triFirstIndex then
			g.triFirstIndex = g.triFirstIndex + 1
		elseif g.triFirstIndex < nti and nti < g.triFirstIndex + g.triCount then
			g.triCount = g.triCount + 1
		end
	end
	if #self.tris*3 ~= self.triIndexes.size then
		error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
	end
end
--]]



-- common interface?  for dif 3d format types?
function Mesh:vtxiter()
	return coroutine.wrap(function()
		for i,v in ipairs(self.vs) do
			coroutine.yield(v)
		end
	end)
end

function Mesh:getTriIndexesForMaterial(groupname)
	if groupname then
		local _, g = self.groups:find(nil, function(g) return g.name == groupname end)
		if g then
			return g.triFirstIndex, g.triFirstIndex + g.triCount - 1
		else
			return 0, -1
		end
	else
		return 0, self.triIndexes.size/3-1
	end
end

-- yields with each material collection for a particular material name
-- default = no name = iterates over all materials
function Mesh:groupiter(groupname)
	return coroutine.wrap(function()
		if groupname then
			local _, g = self.groups:find(nil, function(g) return g.name == groupname end)
			if g then coroutine.yield(g, g.name) end
		else
			for _,g in ipairs(self.groups) do
				coroutine.yield(g, g.name)
			end
		end
	end)
end

-- calculate COM by 0-forms (vertexes)
function Mesh:calcCOM0()
	local result = vec3f()
	for i=0,self.vtxs.size-1 do
		result = result + self.vtxs.v[i].pos
	end
	result = result / self.vtxs.size
	if not math.isfinite(result:normSq()) then
io.stderr:write("couldn't even find the com0\n")
		return vec3f()
	end
	return result
end

-- calculate COM by 1-forms (edges)
-- depend on self.edges being stored
function Mesh:calcCOM1()
	if not self.edges then
		self:findEdges()
	end
	local totalCOM = vec3f()
	local totalLen = 0
	for a,bs in pairs(self.edges) do
		for b in pairs(bs) do
			local v1 = self.vtxs.v[a-1].pos
			local v2 = self.vtxs.v[b-1].pos
			-- volume = *<Q,Q> = *(Q∧*Q) where Q = (b-a)
			-- for 1D, volume = |b-a|
			local length = (v1 - v2):norm()
			local com = (v1 + v2) * .5
			totalCOM = totalCOM + com * length
			totalLen = totalLen + length
		end
	end
	if totalLen == 0 then
		return self:calcCOM0()
	end
	local result = totalCOM / totalLen
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculate COM by 2-forms (triangles)
-- volume = *<Q,Q> = *(Q∧*Q) where Q = (b-a) ∧ (c-a)
-- for 2D, volume = |(b-a)x(c-a)|
function Mesh:calcCOM2(groupname)
	local totalCOM = vec3f()
	local totalArea = 0
	local i1, i2 = self:getTriIndexesForMaterial(groupname)
	for i=i1,i2 do
		local t = self.tris[i+1]
		totalCOM = totalCOM + t.com * t.area
		totalArea = totalArea + t.area
	end
	if totalArea == 0 then
		return self:calcCOM1(groupname)
	end
	local result = totalCOM / totalArea
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculate COM by 3-forms (enclosed volume)
function Mesh:calcCOM3(groupname)
	local totalCOM = vec3f()
	local totalVolume = 0
	local i1, i2 = self:getTriIndexesForMaterial(groupname)
	for i=i1,i2 do
		local t = self.tris[i+1]
		-- using [a,b,c,0] as the 4 pts of our tetrahedron
		-- volume = *<Q,Q> = *(Q∧*Q) where Q = (a-0) ∧ (b-0) ∧ (c-0)
		-- for 3D, volume = det|a b c|
		--local com = (a + b + c) * (1/4)
		local com = t.com * (3/4)

		local volume = t:calcTetradVolume(self)
		totalCOM = totalCOM + com * volume
		totalVolume = totalVolume + volume
	end
	-- if there's no volume then technically this can't exist ... but just fallback
	if totalVolume == 0 then
		return self:calcCOM2(groupname)
	end
	local result = totalCOM / totalVolume
	assert(math.isfinite(result:normSq()))
	return result
end

-- calculates volume bounded by triangles
function Mesh:calcVolume()
	local totalVolume = 0
	for i,t in ipairs(self.tris) do
		totalVolume = totalVolume + t:calcTetradVolume(self)
	end
	if totalVolume < 0 then totalVolume = -totalVolume end
	return totalVolume
end

function Mesh:clearVertexNormals()
	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].normal:set(0,0,0)
	end
end

-- split all indexes so index<->vertex is 1:1
function Mesh:breakAllVertexes()
print('before breakAllVertexes, #vtxs '..self.vtxs.size..' #triindexes '..self.triIndexes.size)
	local nvtxs = vector('MeshVertex_t', self.triIndexes.size)
	local ntris = vector('uint32_t', self.triIndexes.size)
	for i=0,self.triIndexes.size-1 do
		nvtxs.v[i] = self.vtxs.v[self.triIndexes.v[i]]
		ntris.v[i] = i
	end
	self.vtxs = nvtxs
	self.triIndexes = ntris
print('after breakAllVertexes, #vtxs '..self.vtxs.size..' #triindexes '..self.triIndexes.size)

	-- TODO update the mesh ranges as well
	-- assert they do not overlap before
	-- then sort them
	-- then remap them as we break tris

	for i,t in ipairs(self.tris) do
		-- update vertex COMs
		-- they are only valid now
		local com = t.com
		for j=0,2 do
			nvtxs.v[3*(i-1)+j].com = com
		end
	end

	-- tell the next draw to regen the buffer
	-- can I resize a gl arraybuffer?
	self:unloadGL()

	self.edges = nil
	self.edgeIndexBuf = nil
	self.edges2 = nil

	self:calcBBox()
	self:findEdges()
	self:calcCOMs()
end

-- used for traversing loops
function Mesh:getIndexForLoopChain(l)
	local i = l.e[l.v]-1
	assert(i >= 0 and i < self.vtxs.size)
	return i
end
function Mesh:getVtxForLoopChain(l)
	return self.vtxs.v[self:getIndexForLoopChain(l)]
end
function Mesh:getPosForLoopChain(l)
	return self:getVtxForLoopChain(l).pos
end


--[[
find all edges that don't have exactly 2 triangle neighbors.
hmm ... I would like to use this but with the 'edges2' structure ...

hmm hmm maybe I need a mesh with all vertexes merged into their neighboring edges/triangles
and mapping that information back to the original mesh

TODO I might need this but for all edge segments, based on 'edges2' and each subinterval of each triangle edge.
--]]
function Mesh:findBadEdges()
	-- find edges based on vtx comparing pos
	local uniquevs, indexToUniqueV = self:getUniqueVtxs(1e-5)

	-- TODO can I use edges2?  nah because edges2 is only between two triangles ...
	-- TODO edges2 use 'getUniqueVtxs', and then cycle over all tris edges?
	self:findEdges(function(i) return uniquevs[indexToUniqueV[i]] end)

	local border = table()
	local totalEdges = 0
	for a,o in pairs(self.edges) do
		for b,e in pairs(o) do
			if #e.tris == 1 then
				border:insert(e)
			end
			totalEdges = totalEdges + 1
		end
	end

print('edges total', totalEdges, 'border', #border)
assert(#self.tris*3 == self.triIndexes.size)
for i,t in ipairs(self.tris) do assert(t.index == i) end

	-- now put in loops
	local all = table(border)
	local loops = table()
	local lines = table()
	while #all > 0 do
		local loop = table()
		local last = all:remove(1)
--print('first edge', last[1], last[2])
		-- loop traversal / first edge vtx should be based on edge/tri orientation
		-- the loop should go opposite the triangle orientation
		-- for our single tri touching the edge ...
		-- find vtx j on the tri such that tri[j], tri[j+1] == e[1], e[2] , order-independent
		-- then for whatever j+1 is on e, start with that one
		assert(#last.tris == 1, "found an edge which isn't really an edge...")
		local lastvi
		for j=0,2 do
			local ti = last.tris[1].index-1
			local tj1 = uniquevs[indexToUniqueV[self.triIndexes.v[j+3*ti]]]
			local tj2 = uniquevs[indexToUniqueV[self.triIndexes.v[(j+1)%3+3*ti]]]
			local e1 = uniquevs[indexToUniqueV[last[1]-1]]
			local e2 = uniquevs[indexToUniqueV[last[2]-1]]
			if tj1 == e1 and tj2 == e2 then
				assert(not lastvi, "we have a tri with two edges that use the same vtxs...")
				lastvi = 2	-- so we start on 2
			elseif tj1 == e2 and tj2 == e1 then
				assert(not lastvi, "we have a tri with two edges that use the same vtxs...")
				lastvi = 1	-- so we start on 1
			end
		end
		assert(lastvi, "we have a first edge with a single tri which it doesn't touch...")
		loop:insert{v=3-lastvi, e=last}
		while true do
			local found
			for i=1,#all do
				local o = all[i]
--print('checking edge', o[1], o[2])
				for j=1,2 do
					if o[j] == last[lastvi] then
						last = o
						lastvi = 3-j
						loop:insert{v=3-lastvi, e=o}
						all:remove(i)
						found = true
--print('adding edge', last[1], last[2])
						break
					end
				end
				if found then break end
			end
			if not found then
--print('found no more edges, adding to lines')
				lines:insert(loop)
				break
			else
				if last[lastvi] == loop[1].e[loop[1].v] then
--print('reached beginning, adding to loops')
					loops:insert(loop)
					break
				end
			end
		end
	end
print('#loops', #loops)
print('#lines', #lines)

	-- no boundary edges that aren't loops
	-- lines?  how to fix those?
	--if #lines > 0 then error("can't fix stupid") end
	-- luckily I never have to find out (yet)
	-- is this even possible?

	for i,loop in ipairs(loops) do
print('loop #'..i..': '..loop:mapi(function(l) return self:getIndexForLoopChain(l) end):concat', ')
		--[[ determine if the loop is planar (and determine its normal)
		for j=1,#loop-1 do
			local a = self:getPosForLoopChain(loop[j])
			local b = self:getPosForLoopChain(loop[j%#loop+1])
			local c = self:getPosForLoopChain(loop[(j+1)%#loop+1])
			local n = (c - b):cross(b - a)
			print(n)
		end
		--]]
		--[[ just add the tris as-is
-- TODO how to determine loop order ...
-- probably based on normal of opposing edges triangles
if loop[1].e.tris[1][1].v == loop[1].e[1] then
loop = loop:reverse()
end
		for j=2,#loop-1 do
			self.triIndexes:push_back(self:getIndexForLoopChain(loop[1]))
			self.triIndexes:push_back(self:getIndexForLoopChain(loop[j]))
			self.triIndexes:push_back(self:getIndexForLoopChain(loop[j+1]))
		end
		--]]
		--assert(#loop >= 3)
	end

	-- here ... optional?
	-- filter out loops with zero area
	for i=#loops,1,-1 do
		local loop = loops[i]
		local n
		local totalArea = 0
		for j=2,#loop-1 do
			local a = self:getPosForLoopChain(loop[1])
			local b = self:getPosForLoopChain(loop[j])
			local c = self:getPosForLoopChain(loop[j+1])
			local ab = b - a
			local ac = c - a
			local x = ab:cross(ac)
			local xlen = x:norm()
			if xlen > 1e-7 then
				local area
				if not n then
					n = x / xlen
					area = .5 * xlen
--print('normal', n)
				else
					area = .5 * ab:cross(ac):dot(n)
				end
--print('adding', area)
				totalArea = totalArea + area
			end
		end
		if totalArea < 1e-7 then
			-- turns out this comes up zero on some loops with volume (cube with x+ x- y+ removed)
			print('loop #'..i..' has area '..totalArea..' ... ')
--			loops:remove(i)
		else
			print('loop #'..i..' has area '..totalArea..' ... ')
		end
	end

	return loops, lines
end

--[[
doesn't break intersecting tris.
just removes any tris that are internal.
--]]
function Mesh:removeInternalTris()
	--[[
	TODO first break-triangle operation first ... how to break triangles
	otherwise this doesn't go far.
	how to break triangles?
	detct collision
	how to detect collision?
		ensure each line segment of A, projected to B is not within B
		and vice versa.
	what if there is a collision -- how to break?
	--]]

	for i=#self.tris,1,-1 do
		for j=i-1,1,-1 do
			-- TODO ...
		end
	end

	local edges = self:findBadEdges()

	-- second ... merge vertexes
	-- in fact I should look at the merge map of vertexes w/o texcoord or normal condition
	--if not self.bbox then self:calcBBox() end
	--local bboxCornerDist = (self.bbox.max - self.bbox.min):norm()
	--local vtxMergeThreshold = bboxCornerDist * 1e-6
	local vtxMergeThreshold = 1e-5
	local uniquevs, indexToUniqueV = self:getUniqueVtxs(vtxMergeThreshold)

	-- now find edges based on nearest vtx only
	-- TODO separate the edge info from Mesh?
	self:findEdges(function(i)
		return uniquevs[indexToUniqueV[i]]
	end)

	-- finally remove internal tris.
	-- what determines if a triangle is internal?
	-- 1) it needs to have all edges with >2 neighbors.
	-- 2) per-edge, the other two tris planes must have this tri behind them
	-- this won't skip floating edge tris ... those need to be removed separately.
	-- this also won't remove 'internal' tris if there's a hole on the bounding region.
	assert(#self.tris*3 == self.triIndexes.size)
	for i,t in ipairs(self.tris) do
		-- if the triangle intersects another then it needs to break
	end

	self.edges = nil
	self.edgeIndexBuf = nil
	self.edges2 = nil
	self:unloadGL()
end

-- regenerate the vertex normals based on the face normals, weighted average by angle (tesselation-independent and curvature-driven)
function Mesh:generateVertexNormals()
	-- calculate vertex normals
	-- TODO store this?  in its own self.vn2s[] or something?
--print('zeroing vertex normals')
	for i=0,self.vtxs.size-1 do
		self.vtxs.v[i].normal:set(0,0,0)
	end
--print('accumulating triangle normals into vertex normals')
	for i=0,self.triIndexes.size-1,3 do
		local ia = self.triIndexes.v[i]
		local ib = self.triIndexes.v[i+1]
		local ic = self.triIndexes.v[i+2]
		-- not sure what i'm doing with these ...
		-- cache or regen?
		local va = self.vtxs.v[ia]
		local vb = self.vtxs.v[ib]
		local vc = self.vtxs.v[ic]
		local pa = self.vtxs.v[ia].pos
		local pb = self.vtxs.v[ib].pos
		local pc = self.vtxs.v[ic].pos
		local ab = (pb - pa):normalize()
		local bc = (pc - pb):normalize()
		local ca = (pa - pc):normalize()
		local normal = ab:cross(bc):normalize()
		local thetaA = math.acos(math.clamp(-ab:dot(ca),-1,1))
		local thetaB = math.acos(math.clamp(-bc:dot(ab),-1,1))
		local thetaC = math.acos(math.clamp(-ca:dot(bc),-1,1))
		va.normal = va.normal + normal * thetaA
		vb.normal = vb.normal + normal * thetaB
		vc.normal = vc.normal + normal * thetaC
	end
--print('normals vertex normals')
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i]
		local len = v.normal:norm()
		if len > 1e-7 then
			v.normal = v.normal * (1 / len)
		else
			v.normal:set(0,0,0)
		end
--print(k, vtxnormals[i])
	end

	if self.vtxBuf then
		self.vtxBuf
			:bind()
			:updateData(0, ffi.sizeof'MeshVertex_t' * self.vtxs.size, self.vtxs.v)
			:unbind()
	end
end

--[[
in-place clip a mesh by a plane
do so by removing all backfacing triangles
and splitting any overlapping triangles

plane = clip plane (not necessarily normalized)
--]]
function Mesh:clip(plane)
	local modified
if #self.tris*3 ~= self.triIndexes.size then
	error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
end
	for _,g in ipairs(self.groups) do
		for ti=g.triFirstIndex+g.triCount-1,g.triFirstIndex,-1 do
			local t = self.tris[ti+1]
			local tp = self.triIndexes.v + 3*ti
			local vs = range(0,2):mapi(function(j) return self.vtxs.v[tp[j]] end)
			local planeDists = vs:mapi(function(v) return plane:dist(v.pos) end)
			local sides = planeDists:mapi(function(d) return d >= 0 end)
			local frontCount = sides:mapi(function(s) return s and 1 or 0 end):sum()
--print('frontCount', frontCount)
			if frontCount == 3 then
--print('...keep')
				-- keep
			elseif frontCount == 0 then
				modified = true
--print('...remove')
if #self.tris*3 ~= self.triIndexes.size then
	error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
end
				self:removeTri(3*ti)	-- remove
if #self.tris*3 ~= self.triIndexes.size then
	error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
end
			-- needs a new vertex:
			else
				modified = true
				local found
				for j=0,2 do
					if (frontCount == 1 and sides[j+1])
					or (frontCount == 2 and not sides[j+1])
					then
--print('splitting on '..j..'th side')
						local j1 = (j+1)%3
						local j2 = (j1+1)%3
						-- separate off this triangle
						local d1 = planeDists[j1+1] - planeDists[j+1]
						local d2 = planeDists[j2+1] - planeDists[j+1]

						local iv01 = self.vtxs.size
						local nv01 = self.vtxs:emplace_back()
						local s01 = (0 - planeDists[j+1]) / d1
						nv01.pos = math.mix(vs[j+1].pos, vs[j1+1].pos, s01)
						nv01.texcoord = math.mix(vs[j+1].texcoord, vs[j1+1].texcoord, s01)
						nv01.normal = math.mix(vs[j+1].normal, vs[j1+1].normal, s01)

						local iv02 = self.vtxs.size
						local nv02 = self.vtxs:emplace_back()
						local s02 = (0 - planeDists[j+1]) / d2
						nv02.pos = math.mix(vs[j+1].pos, vs[j2+1].pos, s02)
						nv02.texcoord = math.mix(vs[j+1].texcoord, vs[j2+1].texcoord, s02)
						nv02.normal = math.mix(vs[j+1].normal, vs[j2+1].normal, s02)

						local iv0 = tp[j]
						local iv1 = tp[j1]
						local iv2 = tp[j2]
						-- now go from iv0 iv1 iv2
						-- to iv0 iv01 iv02, iv01 iv1 iv2, iv01 iv2 iv02
						-- soo .. replace the current with the first, and insert the other two
						-- this is rotating it to put j at 0
						if frontCount == 1 then	-- shorten the leading side
							tp[j] = iv0
							tp[(j+1)%3] = iv01
							tp[(j+2)%3] = iv02
						else -- replace tp with the base and insert a second base to make a quad
if #self.tris*3 ~= self.triIndexes.size then
	error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
end
							-- insert these into the same material group as we're currently in
							local nti = ti + 1
							tp[0] = iv01
							tp[1] = iv1
							tp[2] = iv2
							-- [[ TODO use insertTri instead
							self.triIndexes:insert(self.triIndexes:begin() + 3*ti + 3, iv01)
							self.triIndexes:insert(self.triIndexes:begin() + 3*ti + 4, iv2)
							self.triIndexes:insert(self.triIndexes:begin() + 3*ti + 5, iv02)
							self.tris:insert(ti+1, Triangle{
								index = nti+1,	-- +1 cuz its' 1-based
							})
							for _,g2 in ipairs(self.groups) do
								if nti <= g2.triFirstIndex then
									g2.triFirstIndex = g2.triFirstIndex + 1
								end
							end
							g.triCount = g.triCount + 1
if #self.tris*3 ~= self.triIndexes.size then
	error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
end
							--]]
							--[[
							self:insertTri(iv01, iv2, iv02, nti)
							--]]
						end
						found = true
						break
					end
				end
				if not found then
					error'here'
				end
			end
		end
	end

if #self.tris*3 ~= self.triIndexes.size then
	error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
end
	self:rebuildTris()
	self:mergeMatchingVertexes()	-- better to merge vtxs than remove empty tris cuz it will keep seams in models
	--self:removeEmptyTris()

if #self.tris*3 ~= self.triIndexes.size then
	error("3*#tris is "..(3*#self.tris).." while triIndexes is "..self.triIndexes.size)
end

	self:unloadGL()
	return modified
end

function Mesh:fillHoles()
print('Mesh:fillHoles begin')
	local loops, lines = self:findBadEdges()

	-- just add it to the last group
	local _, g = self.groups:find(nil, function(g)
		return (g.triFirstIndex + g.triCount) * 3 == self.triIndexes.size
	end)
	assert(g, "are you sure you have any groups in this mesh?")

	for i,loop in ipairs(loops) do
print('loop #'..i..': '..loop:mapi(function(l) return self:getIndexForLoopChain(l) end):concat', ')
		-- [[ determine if the loop is planar (and determine its normal)
		local planenormal
		local planeorigin = self:getPosForLoopChain(loop[1])
		for j=1,#loop-1 do
			local a = self:getPosForLoopChain(loop[j])
			local b = self:getPosForLoopChain(loop[j%#loop+1])
			local c = self:getPosForLoopChain(loop[(j+1)%#loop+1])
			local n, len = (b - a):cross(c - a):unitOrZero()
			if len > 1e-1 then
				-- should I average them ?  or should I enforce that they all match?
				if not planenormal then
					planenormal = n
				else
					if math.abs(planenormal:dot(n)) < 1 - 1e-1 then
						io.stderr:write("old normal was "..planenormal..", new normal is "..n..", loop is not planar\n")
					end
				end
			end
		end
		if not planenormal then
			return false, "plane normal not found"
		end
		local ex, ey = planenormal:perpendicular2()
print('plane basis ex ey n', ex, ey, planenormal)
		--]]
		-- [[ just add the tris as-is
		--[=[ TODO how to determine loop order ...
		-- probably based on normal of opposing edges triangles
		if loop[1].e.tris[1][1].v == loop[1].e[1] then
		end
		--]=]
		-- TODO when to reverse ...
		-- this should be based on which side is inside
		-- how to detect that?
		-- ray test from loop center in direction of loop normal
		--loop = loop:reverse()

		-- now I need a basis point (loop[0] works)
		-- and I need a basis vector (orthogonal to plane normal works)
		-- and I need to know the handedness of the edge loop around the vector
		-- and then I need to sort along one basis vector
		-- track edges
		-- and fill in rhombuses as I go
print'calc uv'
		for _,l in ipairs(loop) do
			local d = self:getPosForLoopChain(l) - planeorigin
			l.uv = vec2f(d:dot(ex), d:dot(ey))
		end

		--[=[ naive fan implementation - only works on convex polygons
		-- TODO pick origin of fan as a corner and reduce # of tris (or gen a lot of 0-area tris to be reduced later)
		-- TODO this only works for convex polygons ...what about concave shapes? gets more complicated.
		-- TODO TODO for that, sweep across the poly, keep track of edges, do just like with software rendering
print'adding indices'
		for j=2,#loop-1 do
			self.triIndexes:push_back(self:getIndexForLoopChain(loop[1]))
			self.triIndexes:push_back(self:getIndexForLoopChain(loop[j]))
			self.triIndexes:push_back(self:getIndexForLoopChain(loop[j+1]))
			g.triCount = g.triCount + 1
		end
		--]=]
		-- [=[ earcut
		local indata = range(#loop*2):mapi(function(l,i)
			return loop[bit.rshift(i-1,1)+1].uv.s[bit.band(i-1,1)]
		end)
print('indata', require 'ext.tolua'(indata))
		local outdata = require 'mesh.earcut'(indata, {})
print('outdata', require 'ext.tolua'(outdata))
		assert(#outdata % 3 == 0)
		-- TODO when to reverse ...
		for ti=0,#outdata/3-1 do
			for k=0,2 do
				local j = 2-k+3*ti+1
				local o = outdata[j]
print('adding to loop-index', o, 'vtx index', self:getIndexForLoopChain(loop[o]), 'uv', loop[o].uv, 'pos', self:getPosForLoopChain(loop[o]))
				self.triIndexes:emplace_back()[0] = self:getIndexForLoopChain(loop[o])
			end
		end
		g.triCount = g.triCount + #outdata/3
		--]=]
		--]]
		assert(#loop >= 3)
	end

	self:unloadGL()
	self:rebuildTris()
print('Mesh:fillHoles end')
end

-- 'fwd' is used for depth calculation, 'dir' is the ray direction
function Mesh:findClosestVertexToMouseRay(pos, dir, fwd, cosEpsAngle)
	-- assumes dir is 1 unit fwd along the view fwd
	--dir = dir:normalize()
	local dirlen = dir:norm()
	local bestdot, besti, bestdepth
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i].pos
		local delta = v - pos
		local depth = delta:dot(fwd)
		--local dot = dir:dot(delta) / (delta:norm() * dirlen)
		local dot = dir:unit():dot(delta:unit())
		if dot > cosEpsAngle then
			if not bestdepth
			or depth < bestdepth
			then
				besti = i
				bestdepth = depth
				bestdot = dot
			end
		end
	end
	return besti, bestdepth
end

function Mesh:findClosestTriToMouseRay(pos, dir, fwd, cosEpsAngle)
	-- assumes dir is 1 unit fwd along the view fwd
	--dir = dir:normalize()
	local dirlen = dir:norm()
	local besti, bestdist
	assert(#self.tris * 3 == self.triIndexes.size)
	for ti,t in ipairs(self.tris) do
		local i = 3*(ti-1)
		local tnormal, area = t.normal, t.area

		local a,b,c = t:vtxPos(self)
		local planePt = a
		if area > 1e-7 then
			-- make sure it's pointing towards the ray origin
			if tnormal:dot(dir) < 0 then tnormal = -tnormal end

			local p, s = plane3f():fromDirPt(tnormal, planePt):intersectRay(pos, dir)
			if s >= 0 and (not bestdist or s < bestdist) then
				-- barycentric coordinates
				if t:insideBCC(p, self) then
					besti = i
					bestdist = s
				end
			end
		end
	end
	return besti, bestdist
end


-- all the draw functionality is tied tightly with view.lua so ...
-- idk if i should move it from one or the other


-- upon ctor the images are loaded (in case the caller isn't using GL)
-- so upon first draw - or upon manual call - load the gl textures
function Mesh:loadGL(shader)
	if self.loadedGL then return end
	self.loadedGL = true

	local gl = require 'gl'
	local glreport = require 'gl.report'
	local GLTex2D = require 'gl.tex2d'
	local GLArrayBuffer = require 'gl.arraybuffer'
	local GLAttribute = require 'gl.attribute'
	local GLVertexArray = require 'gl.vertexarray'

	-- load textures
	for _,g in ipairs(self.groups) do
		if g.image_Kd
		and not g.tex_Kd
		then
			g.tex_Kd = GLTex2D{
				image = g.image_Kd,
				minFilter = gl.GL_NEAREST,
				magFilter = gl.GL_LINEAR,
			}:unbind()
		end
	end

--print('creating array buffer of size', self.vtxs.size)
	if shader and not self.vtxBuf then
		self.vtxBuf = GLArrayBuffer{
			size = self.vtxs.size * ffi.sizeof'MeshVertex_t',
			data = self.vtxs.v,
			usage = gl.GL_STATIC_DRAW,
		}:unbind()
glreport'here'

		self.vtxAttrs = table{
			{name='pos', size=3},
			{name='texcoord', size=3},
			{name='normal', size=3},
			{name='com', size=3},
		}:mapi(function(info)
			if not shader.attrs[info.name] then return end
			return GLAttribute{
				buffer = self.vtxBuf,
				size = info.size,
				type = gl.GL_FLOAT,
				stride = ffi.sizeof'MeshVertex_t',
				offset = ffi.offsetof('MeshVertex_t', info.name),
			}, info.name
		end)
		shader:use()
glreport'here'
		self.vao = GLVertexArray{
			program = shader,
			attrs = self.vtxAttrs,
		}
		shader:setAttrs(self.vtxAttrs)
		shader:useNone()
glreport'here'
	end
end

function Mesh:unloadGL()
	self.loadedGL = nil

	-- when loading/unloading/calling glGenBuffers too many times (even with subsequent glDeleteBuffers) I'm getting sporatic GL_INVALID_OPERATIONS upon glGenBuffers ... even if it's just the 10th or so call.  wtf.
	if self.vao then self.vao:release() end
	if self.vtxBuf then self.vtxBuf:release() end

	self.vao = nil
	self.vtxBuf = nil
	self.vtxAttrs = nil
end

function Mesh:draw(args)
	local gl = require 'gl'
	self:loadGL(args.shader)	-- load if not loaded
	
	local method = args.method
	if not method then method = 'vao' end
	if method == 'vao' and not self.vao then method = nil end
	if not method then method = 'attribarray' end
	if method == 'attribarray' and not args.shader then method = nil end

	-- [[ vao ... getting pretty tightly coupled with the view.lua file ...
	if method == 'vao' then
		assert(self.vao)
		self.vao:bind()
		for _,g in ipairs(self.groups) do
			if args.beginGroup then args.beginGroup(g) end
			-- TODO use GLElementArrayBuffer?
			if g.triCount > 0 then
				gl.glDrawElements(gl.GL_TRIANGLES, g.triCount * 3, gl.GL_UNSIGNED_INT, self.triIndexes.v + g.triFirstIndex * 3)
			end
			if args.endGroup then args.endGroup(g) end
		end
		self.vao:unbind()
	--]]
	-- [[ vertex attrib pointers ... requires specifically-named attrs in the shader
	elseif method == 'attribarray' then
		assert(args.shader)
		gl.glVertexAttribPointer(args.shader.attrs.pos.loc, 3, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].pos.s)
		gl.glVertexAttribPointer(args.shader.attrs.texcoord.loc, 3, gl.GL_FLOAT, gl.GL_FALSE, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].texcoord.s)
		gl.glVertexAttribPointer(args.shader.attrs.normal.loc, 3, gl.GL_FLOAT, gl.GL_TRUE, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].normal.s)
		gl.glEnableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.texcoord.loc)
		gl.glEnableVertexAttribArray(args.shader.attrs.normal.loc)
		for _,g in ipairs(self.groups) do
			if args.beginGroup then args.beginGroup(g) end
			if g.triCount > 0 then
				gl.glDrawElements(gl.GL_TRIANGLES, g.triCount * 3, gl.GL_UNSIGNED_INT, self.triIndexes.v + g.triFirstIndex * 3)
			end
			if args.endGroup then args.endGroup(g) end
		end	
		gl.glDisableVertexAttribArray(args.shader.attrs.pos.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.texcoord.loc)
		gl.glDisableVertexAttribArray(args.shader.attrs.normal.loc)
	--]]
	else
		local curtex
		for _,g in ipairs(self.groups) do
			--[[
			if g.Kd then
				gl.glColor4f(g.Kd:unpack())
			else
				gl.glColor4f(1,1,1,1)
			end
			--]]
			-- [[
			if g
			and g.tex_Kd
			and not (args and args.disableTextures)
			then
				-- TODO use .Ka, Kd, Ks, Ns, etc
				-- with fixed pipeline?  opengl lighting?
				-- with a shader in the wavefrontobj lib?
				-- with ... nothing?
				curtex = g.tex_Kd
				curtex:enable()
				curtex:bind()
			else
				if curtex then
					curtex:unbind()
					curtex:disable()
					curtex = nil
				end
			end
			--]]
			if args.beginGroup then args.beginGroup(g) end

			if g.triCount > 0 then
				-- [[ vertex client arrays
				gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].pos.s)
				gl.glTexCoordPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].texcoord.s)
				gl.glNormalPointer(gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].normal.s)
				gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
				gl.glEnableClientState(gl.GL_TEXTURE_COORD_ARRAY)
				gl.glEnableClientState(gl.GL_NORMAL_ARRAY)
				gl.glDrawElements(gl.GL_TRIANGLES, g.triCount * 3, gl.GL_UNSIGNED_INT, self.triIndexes.v + g.triFirstIndex * 3)
				gl.glDisableClientState(gl.GL_VERTEX_ARRAY)
				gl.glDisableClientState(gl.GL_TEXTURE_COORD_ARRAY)
				gl.glDisableClientState(gl.GL_NORMAL_ARRAY)
				--]]
				--[[ immediate mode
				gl.glBegin(gl.GL_TRIANGLES)
				for i=0,3*g.triCount-1 do
					local vi = self.triIndexes.v[i + g.triFirstIndex * 3]
					local v = self.vtxs.v[vi]
					gl.glTexCoord2fv(v.texcoord.s)
					gl.glNormal3fv(v.normal.s)
					gl.glVertex3fv(self.vtxs.v[vi].pos.s)
				end
				gl.glEnd()
				--]]
			end
			if args.endGroup then args.endGroup(g) end
		end
		-- [[
		if curtex then
			curtex:unbind()
			curtex:disable()
		end
		--]]
	end
	require 'gl.report''here'
end

-- make sure my edges match my faces
-- can't handle group explode dist because edges aren't stored associted with materials ...
-- they are per-tri, which is per-face, which is per-material, but there can be multiple materials per edge.
function Mesh:drawEdges(triExplodeDist, groupExplodeDist)
	local gl = require 'gl'

	if not self.edgeIndexBuf then
		self:findEdges()
	end

	--gl.glLineWidth(3)
	gl.glColor3f(1,1,0)

	-- TODO shader that does the explode stuff
	gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].pos.s)
	gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
	gl.glDrawElements(gl.GL_LINES, self.edgeIndexBuf.size, gl.GL_UNSIGNED_INT, self.edgeIndexBuf.v)
	gl.glDisableClientState(gl.GL_VERTEX_ARRAY)

	--gl.glLineWidth(1)
end

function Mesh:drawVertexes(triExplodeDist, groupExplodeDist)
	local gl = require 'gl'
	gl.glColor3f(1,1,1)
	gl.glPointSize(3)

	-- TODO shader that does the explode stuff
	gl.glVertexPointer(3, gl.GL_FLOAT, ffi.sizeof'MeshVertex_t', self.vtxs.v[0].pos.s)
	gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
	gl.glDrawArrays(gl.GL_POINTS, 0, self.vtxs.size)
	gl.glDisableClientState(gl.GL_VERTEX_ARRAY)

	gl.glPointSize(1)
end

function Mesh:drawVertexNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for i=0,self.vtxs.size-1 do
		local v = self.vtxs.v[i]
		gl.glVertex3f(v.pos:unpack())
		gl.glVertex3f((v.pos + v.normal):unpack())
	end
	gl.glEnd()
end

function Mesh:drawTriNormals()
	local gl = require 'gl'
	gl.glColor3f(0,1,1)
	gl.glBegin(gl.GL_LINES)
	for i,t in ipairs(self.tris) do
		gl.glVertex3fv(t.com.s)
		gl.glVertex3fv((t.com + t.normal).s)
	end
	gl.glEnd()
end

-- works with mesh:generateTriBasis
function Mesh:drawTriBasis()
	local gl = require 'gl'
	gl.glLineWidth(3)
	gl.glBegin(gl.GL_LINES)
	for i,t in ipairs(self.tris) do
		if t.basis then
			gl.glColor3f(1,0,0)
			gl.glVertex3f(t.com:unpack())
			gl.glVertex3f((t.com + t.basis[1]):unpack())
			gl.glColor3f(0,1,0)
			gl.glVertex3f(t.com:unpack())
			gl.glVertex3f((t.com + t.basis[2]):unpack())
			gl.glColor3f(0,0,1)
			gl.glVertex3f(t.com:unpack())
			gl.glVertex3f((t.com + t.basis[3]):unpack())
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

-- works with mesh:calcTriSurfaceGroups
-- draw lines of triGroups[]  .borderEdges[]
-- this duplicates drawUnwrapUVEdges except for the mesh.triGroups
function Mesh:drawTriSurfaceGroupEdges(highlightEdge)
	if not self.triGroups then
		self:calcTriSurfaceGroups()
	end
	local gl = require 'gl'
	gl.glLineWidth(3)
	gl.glEnable(gl.GL_BLEND)
	gl.glDepthMask(gl.GL_FALSE)
	gl.glBegin(gl.GL_LINES)
	for _,tg in ipairs(self.triGroups) do
		for _,info in ipairs(tg.borderEdges) do
			local e = info.edge
			local alpha = e == highlightEdge and 1 or .3
			if e.isExtEdge == nil then
				gl.glColor4f(1,0,0, alpha)	-- edgeInstances
			elseif e.isExtEdge == false then
				gl.glColor4f(0,1,0, alpha)	-- cornerInstances / concaveInstances
			else
				gl.glColor4f(0,0,1, alpha)	-- cornerInstances / convexInstances
			end
			local v1, v2 = e:getPts()
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

function Mesh:drawTriSurfaceGroupPlanes()
	if not self.triGroups then
		self:calcTriSurfaceGroups()
	end
	local gl = require 'gl'
	gl.glEnable(gl.GL_BLEND)
	gl.glDepthMask(gl.GL_FALSE)
	gl.glDisable(gl.GL_CULL_FACE)
	gl.glColor4f(1,1,0,.1)
	gl.glBegin(gl.GL_QUADS)
	for _,g in ipairs(self.triGroups) do
		for _,info in ipairs(g.borderEdges) do
			local e = info.edge
			local v1, v2 = e:getPts()
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
	for _,g in ipairs(self.triGroups) do
		for _,info in ipairs(g.borderEdges) do
			local e = info.edge
			gl.glVertex3f((e.com + e.normAvg):unpack())
			gl.glVertex3f((e.com + e.normAvg + info.clipPlane.n * .5):unpack())
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)
end

-- TODO this won't draw correctly
-- because it's showing the normAvg ... not the clipPlane of the info
-- fwiw neither will the other debug draw clip plane function just above
-- both need a perpendicular basis here
function Mesh:drawTriGroupEdgeClipPlanes(highlightEdge)
	if not self.edgeClipGroups then
		self:calcTriEdgeGroups()
	end
	local gl = require 'gl'
	gl.glDisable(gl.GL_DEPTH_TEST)
	gl.glEnable(gl.GL_BLEND)

	local normalExtrusionEpsilon  = .1
	-- draw along the fake-edge, then turn and draw along its clip-plane
	gl.glLineWidth(3)
	gl.glBegin(gl.GL_LINES)
	for _,eg in ipairs(self.edgeClipGroups) do
		local alpha = eg.srcEdges:find(nil, function(es)
			return es.edge == highlightEdge
		end) and 1 or .3
		for _,info in ipairs(eg.borderEdges) do
			local e = info.edge
			-- if e.plane.n is const but info.clipPlane.n alternates dir then this will alternate its direction:
			--local n = e.plane.n:cross(info.clipPlane.n)
			-- so instead ...
			local n = table():append(eg.srcEdges:mapi(function(es)
				return table.mapi(es.edge.tris, function(t) return t.normal end)
			end):unpack()):sum():normalize()
			-- [[ draw edge
			gl.glColor4f(1,0,0,alpha)
			gl.glVertex3f((e.planePos + normalExtrusionEpsilon * n):unpack())
			gl.glVertex3f((e.planePos + normalExtrusionEpsilon * n + e.plane.n * .5):unpack())
			--]]
			-- [[ draw normal
			gl.glColor4f(0,1,1,alpha)
			gl.glVertex3f((e.planePos + normalExtrusionEpsilon * n + e.plane.n * .5):unpack())
			gl.glVertex3f((e.planePos + normalExtrusionEpsilon * n + e.plane.n * .5 + info.clipPlane.n * .25):unpack())
			--]]
		end
	end
	gl.glEnd()
	gl.glLineWidth(1)

	-- [[ draw from real plane to fake plane?
	gl.glDisable(gl.GL_CULL_FACE)
	gl.glBegin(gl.GL_TRIANGLES)
	for _,eg in ipairs(self.edgeClipGroups) do
		local alpha = eg.srcEdges:find(nil, function(es)
			return es.edge == highlightEdge
		end) and 1 or .3
		for _,info in ipairs(eg.borderEdges) do
			local e = info.edge
			local n = e.plane.n:cross(info.clipPlane.n)	-- n is solely for lifting off the plane ... TODO use e.basis[3] ?
			gl.glColor4f(1,0,0,alpha)
			gl.glVertex3f((1e-3 * n + e.planePos + e.plane.n * .05 + info.clipPlane.n * .05):unpack())
			gl.glColor4f(0,1,0,alpha)
			gl.glVertex3f((1e-3 * n + e.planePos + e.plane.n * .3 + info.clipPlane.n * .05):unpack())
			-- move back along clip plane normal ...
			--gl.glVertex3f((e.planePos + 1e-3 * n + e.plane.n * .3 + info.clipPlane.n * .25):unpack())
			-- or just use the COM
			gl.glColor4f(0,0,1,alpha)
			gl.glVertex3f((1e-3 * n + e.com):unpack())
		end
	end
	gl.glEnd()
	--]]
end

return Mesh
