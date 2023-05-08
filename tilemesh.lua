--[[
mesh = mesh with texcoords
omesh = mesh to tile

mesh is modified
--]]
local function tileMesh(mesh, omesh)
	-- for each tri
	for i=0,mesh.triIndexBuf.size-3,3 do
		-- find uv min max
		-- maybe stretch bounds to include edges of placements?
		-- interpolate across uv
		-- find lattice locations where an instance should be placed 
		-- place mesh
		local uvmin = vec2f(math.huge, math.huge)
		local uvmax = -uvmin
		for j=0,2 do
			
		end
	end
end
return tileMesh
