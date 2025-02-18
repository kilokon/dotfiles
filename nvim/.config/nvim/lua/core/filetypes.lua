vim.filetype.add({
	extension = {
		conf = "toml",
		h = "c",
		cpp = "cpp",
	},
	filename = {
		[".editorconfig"] = "ini",
		["setup.cfg"] = "toml",
		["gitconfig"] = "gitconfig",
	},
	-- pattern = {
	-- 	[".*"] = {
	-- 		function(_, bufnr)
	-- 			local content = vim.api.nvim_buf_get_lines(bufnr, 0, 1, true)[1]
	-- 			if content:match("^#%%Module") then
	-- 				return "tcl"
	-- 			end
	-- 		end,
	-- 	},
	-- },
})

-- vim.treesitter.language.register('groovy', 'Jenkinsfile')
