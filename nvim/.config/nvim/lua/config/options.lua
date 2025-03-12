local opt = vim.opt
local g = vim.g

-- Set <space> as the leader key
g.mapleader = " "
g.maplocalleader = " "

opt.clipboard:append({ "unnamed", "unnamedplus" })

vim.o.number = true
vim.o.relativenumber = true
vim.o.signcolumn = "yes"

opt.tabstop = 2 -- Number of spaces that a <Tab> in the file counts for.
opt.shiftwidth = 2 -- Number of spaces to use for each step of (auto)indent.
vim.o.softtabstop = 4
vim.o.expandtab = true
opt.cursorline = true -- Highlight the text line of the cursor

vim.o.splitbelow = true
vim.o.splitright = true

vim.o.smartcase = true
vim.o.ignorecase = true

-- Temp Files
opt.backup = false
opt.writebackup = false
opt.swapfile = false

-- undo
opt.undofile = true
opt.undolevels = 10000

-- folds
vim.o.foldcolumn = "0"
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

vim.o.swapfile = false
vim.o.updatetime = 200
vim.o.laststatus = 3

-- Completion
vim.opt.completeopt = { "menu", "menuone", "noselect" }
vim.opt.pumheight = 10

-- lsp
vim.lsp.set_log_level("error")

-- vim.o.mouse = ""
