local vec4x4f = require 'vec-ffi.vec4x4f'

local function translateMat4x4(t)
	return vec4x4f():setTranslate(t.x, t.y, t.z)
end

local function matrix3x3To4x4(b)
	local m = vec4x4f()
	for i=0,2 do
		for j=0,2 do
			m.ptr[i + 4 * j] = b[j+1].s[i]
		end
	end
	m.ptr[3 + 4 * 3] = 1
	return m
end

return {
	translateMat4x4 = translateMat4x4,
	matrix3x3To4x4 = matrix3x3To4x4,
}
