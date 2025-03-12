vim.g.mapleader = " "
vim.g.maplocalleader = " "
local opts = { noremap = true, silent = true }

-- qq -> Quit
vim.keymap.set("n", "qq", "<C-W>q")

-- occasionally mistype :W
vim.api.nvim_create_user_command("W", "w", { bang = true })

-- vv -> vertical split
vim.keymap.set("n", "vv", "<C-W>v")
-- hh - Makes horizontal split
vim.keymap.set("n", "hh", "<C-W>s")

-- Jumping between splits
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-l>", "<C-w>l")

-- Cancel search highlighting with ESC
vim.keymap.set("n", "<ESC>", ":nohlsearch<Bar>:echo<CR>")

-- Indentation
vim.keymap.set("v", "<", "<gv", opts)
vim.keymap.set("v", ">", ">gv", opts)

-- Move down and up only one visual line
vim.keymap.set("n", "j", "v:count==0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set("n", "k", "v:count==0 ? 'gk' : 'k'", { expr = true, silent = true })

vim.keymap.set("n", "<leader>bb", "<cmd>lua require('telescope.builtin').buffers()<CR>")

-- traversing buffers
vim.keymap.set("n", "]b", "<cmd>bnext<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "[b", "<cmd>bprevious<cr>", { desc = "Previous Buffer" })
