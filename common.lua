local matrix_ffi = require 'matrix.ffi'
-- some functions that maybe should go to matrix_ffi

local function translateMat4x4(t)
	-- TODO this is transpose the initialization of matrix_ffi.setTranslate ... sooo which is right?
	return matrix_ffi({
		{1,0,0,t.x},
		{0,1,0,t.y},
		{0,0,1,t.z},
		{0,0,0,1},
	}, 'float')
end

local function matrix3x3To4x4(b)
	-- matrix_ffi stores col-major
	local m = matrix_ffi({4,4}, 'float'):zeros()
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
