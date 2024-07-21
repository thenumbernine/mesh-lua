package = "mesh"
version = "dev-1"
source = {
	url = "git+https://github.com/thenumbernine/mesh-lua"
}
description = {
	summary = "Mesh library for LuaJIT.",
	detailed = "Mesh library for LuaJIT.",
	homepage = "https://github.com/thenumbernine/mesh-lua",
	license = "MIT"
}
dependencies = {
	"lua >= 5.1",
}
build = {
	type = "builtin",
	modules = {
		["mesh.chopupboxes"] = "chopupboxes.lua",
		["mesh.chopupboxes2"] = "chopupboxes2.lua",
		["mesh.clipcube"] = "clipcube.lua",
		["mesh.combine"] = "combine.lua",
		["mesh.common"] = "common.lua",
		["mesh.earcut"] = "earcut.lua",
		["mesh.edgegraph"] = "edgegraph.lua",
		["mesh.filtermtls"] = "filtermtls.lua",
		["mesh"] = "mesh.lua",
		["mesh.objloader"] = "objloader.lua",
		["mesh.resave"] = "resave.lua",
		["mesh.tilemesh"] = "tilemesh.lua",
		["mesh.tileview"] = "tileview.lua",
		["mesh.unwrapuvs"] = "unwrapuvs.lua",
		["mesh.view"] = "view.lua"
	}
}
