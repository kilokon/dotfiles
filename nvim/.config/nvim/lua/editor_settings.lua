--Dont know lua equivalents
vim.cmd "set noshowcmd"
vim.cmd "set noshowmode"
vim.cmd "set hidden"
vim.cmd "set nowrap"
vim.cmd "set nojoinspaces"

local options = {
-- Using wider tabs	
	number=true,
	shiftwidth=8,
	softtabstop=8,
	tabstop=8,
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
        mouse = 'a',
        scrolloff=2,

-- ui
        numberwidth = 4,

-- text
        encoding='utf-8',

 }



for k, v in pairs(options) do
	vim.opt[k] = v
end



-- Wrapping options
vim.opt.formatoptions=tc, -- wrap text and comments using textwidth
vim.opt.formatoptions:append("r") -- continue comments when pressing ENTER in I mode
vim.opt.formatoptions:append("q") -- enable formatting of comments with gq
vim.opt.formatoptions:append("n") -- detect lists for formatting
vim.opt.formatoptions:append("b") -- auto-wrap in insert mode, and do not wrap old long lines

