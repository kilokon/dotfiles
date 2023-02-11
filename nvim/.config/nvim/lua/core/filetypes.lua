vim.filetype.add({
	extension = {
		png = "image",
		jpg = "image",
		jpeg = "image",
		exr = "image",
		gif = "image",
		es6 = "javascript",
		kbd = "lisp"
	},
	filename = {
		[".envrc"] = "sh",
	},
	pattern = {
		[".*config/git/config"] = "gitconfig",
		[".env.*"] = "sh",
	},
})
