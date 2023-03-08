local function map(mode, lhs, rhs, opts)
        local options = { noremap = true, silent = true }
        if opts then
                options = vim.tbl_extend("force", options, opts)
        end
        vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local opts = { noremap = true, silent = true }

local opts_n = require("drystuff.noremaps").map_options("n")
local wk = require("which-key")

--Remap space as leader key
map("n", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

--smartquit
map("n", "qq", '<cmd>lua require("core.smart_quit")()<CR>')

-- Cancel search highlighting with ESC
map("n", "<ESC>", ":nohlsearch<Bar>:echo<CR>", opts)




-- map("n", "s", "<Nop>", opts)
-- map("n", ",", "<Nop>", opts)




map("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
map("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
map("v", "<A-j>", ":m .+1<CR>==", opts)
map("v", "<A-k>", ":m .-2<CR>==", opts)
-- map("v", "p", '"_dP', opts)

-- Map Esc to kk
map("i", "jk", "<Esc>")

-- vv to generate new vertical split
map("n", "vv", "<C-w>v")

-- Navigate Splits
map("n", "<S-Up>", "<C-w>k", opts)
map("n", "<S-Down>", "<C-w>j", opts)
map("n", "<S-Left>", "<C-w>h", opts)
map("n", "<S-Right>", "<C-w>l", opts)

-- Clear search highlighting with <leader> and c
-- map("n", "<leader>c", ":nohl<CR>")


-- Toggle code folds.
map("n", ",", '<Cmd>silent! execute "normal! za"<CR>')

-- Visual --
-- Stay in indent mode
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- Save file with ctrl-s in inser mode
-- map("i", "<C-s>", "<c-o>:w<cr>")

-- Undo in inser mode
--

map("n", "gt", "<cmd>:tabnext<cr>", opts)
-- map("n", "gT", "<cmd>:tabnext<cr>", opts)


local mappings = {
        -- ["w"] = { "<cmd>w!<CR>", "Save" },
        -- ["/"] = { "<Plug>(comment_toggle_linewise_current)", "Comment toggle current line" },
        -- ["q"] = { "<cmd>BufferKill<CR>", "Close Buffer" },
        -- ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },
        -- ["e"] = { "<cmd>Ex<CR>", "Explorer" },
        U = {
                name = "Updates",
                p = { "<cmd>PackerSync<cr>", "PackerSync" },
                t = { "<cmd>TSUpdate<cr>", "TreeSitter Update" }
        }
}

--map("n", "<leader>pv", ":Ex<CR>", opts)


wk.register(mappings, opts_n)
