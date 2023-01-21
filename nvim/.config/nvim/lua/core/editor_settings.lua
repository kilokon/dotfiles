local providers = {
        loaded_node_provider = 0,
        loaded_perl_provider = 0,
        loaded_ruby_provider = 0,
}

for ii, jj in pairs(providers) do
        vim.g[ii] = jj
end

local options = {
	-- Using wider tabs
	number = true,
	shiftwidth = 8,
	softtabstop = 8,
	tabstop = 8,
	expandtab = true,

	-- backup & autosave
	-- 'backup' 'writebackup'	action	~
	-- off	     off	no backup made
	-- off	     on		backup current file, deleted afterwards (default)
	-- on	     off	delete old backup, backup current file
	-- on	     on		delete old backup, backup current file
	backup = false,
	writebackup = false,
	swapfile = false,

	-- mouse
	mouse = "a",
	scrolloff = 2,

	-- ui
	numberwidth = 4,

	-- text
	encoding = "utf-8",
        ignorecase = true,
        clipboard = "unnamedplus",
        --hover
        --hover_with_actions = true, 
}

for k, v in pairs(options) do
	vim.opt[k] = v
end

