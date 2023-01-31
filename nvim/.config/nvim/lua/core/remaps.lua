local function map(mode, lhs, rhs, opts)
        local options = { noremap = true, silent = true }
        if opts then
                options = vim.tbl_extend("force", options, opts)
        end
        vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local opts = { noremap = true, silent = true }

--smartquit
map("n", "qq", '<cmd>lua require("core.smart_quit")()<CR>')

-- Cancel search highlighting with ESC
map("n", "<ESC>", ":nohlsearch<Bar>:echo<CR>", opts)

--Remap space as leader key
-- map("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "


map("n", "<leader>pv", ":Ex<CR>", opts)
map("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
map("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
map("v", "<A-j>", ":m .+1<CR>==", opts)
map("v", "<A-k>", ":m .-2<CR>==", opts)
-- map("v", "p", '"_dP', opts)

-- Map Esc to kk
map("i", "kk", "<Esc>")

-- vv to generate new vertical split
map("", "vv", "<C-w>v")

-- Clear search highlighting with <leader> and c
map("n", "<leader>c", ":nohl<CR>")

-- Visual --
-- Stay in indent mode
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- Save file with ctrl-s in inser mode
map("i", "<C-s>", "<c-o>:w<cr>")

-- Undo in inser mode
