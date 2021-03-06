--local term_opts = { silent = true }

-- Shorten function name

--local keymap = vim.api.nvim_set_keymap

-- use space as a the leader key
--vim.g.mapleader = ' '
-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

local function map(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end
local opts = { noremap = true, silent = true }

--smartquit
map("n", "qq", '<cmd>lua require("utils/smart_quit")()<CR>')

-- Cancel search highlighting with ESC
map("n", "<ESC>", ":nohlsearch<Bar>:echo<CR>", opts)

--Remap space as leader key
map("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Disable arrow keys
-- map("", "C-<up>", "<nop>")
-- map("", "C-<down>", "<nop>")
-- map("", "C-<left>", "<nop>")
-- map("", "C-<right>", "<nop>")

--map("n", "<A-<right>>", "<nop>")
--map("n", "<A-<left>>", "<nop>")

-- Navigate buffers with left/right keys
--map("n", "A-<right>", ":bn<CR>")
--map("n", "A-<left>", ":bp<CR>")
--map("n", "C-p", ":")
--map("v", "A-<right>", ":bn<CR>")
-- Move text up and down
-- Move text up and down
map("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
map("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
map("v", "<A-j>", ":m .+1<CR>==", opts)
map("v", "<A-k>", ":m .-2<CR>==", opts)
map("v", "p", '"_dP', opts)

-- copy from and to system clipboard
map("v", "<Leader>y", '"+y')
map("n", "<C-y>", '"+p')
map("n", "<Leader>P", '"+P')

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

-----------------------------------------------------------
-- Applications and Plugins shortcuts
-----------------------------------------------------------
--tab-barbar
-- Move to previous/next
map("n", "<A-,>", "<Cmd>BufferPrevious<CR>", opts)
map("n", "<A-.>", "<Cmd>BufferNext<CR>", opts)
-- Close buffer
map("n", "<A-c>", "<Cmd>BufferClose<CR>", opts)
-- Magic buffer-picking mode
map("n", "<C-p>", "<Cmd>BufferPick<CR>", opts)

-- Terminal mappings
map("n", "<C-t>", ":Term<CR>", { noremap = true }) -- open
map("t", "<Esc>", "<C-\\><C-n>") -- exit

-- NvimTree
map("n", "<C-n>", ":NvimTreeToggle<CR>") -- open/close
map("n", "<leader>f", ":NvimTreeRefresh<CR>") -- refresh
map("n", "<leader>n", ":NvimTreeFindFile<CR>") -- search file

-- WhichKey
-- map("", "<leader>", ":WhichKey<leader><CR>", { noremap = true })

-- LSP
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", { noremap = true })
map(
  "n",
  "<Leader>ld",
  "<cmd>LspTrouble lsp_definitions<CR>",
  { noremap = true }
)
-- map('n','<up>', "<cmd>RustMoveItemUp<CR>", { noremap = true})
-- map('n','<down>', "<cmd>RustMoveItemDown<CR>", { noremap = true})
-- map('n','<down>', "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", {noremap = true})

-- Rust Tools
map("n", "rr", "<cmd>RustHoverActions<CR><cmd>RustHoverActions<CR>")
map("n", "rc", "<cmd>RustOpenCargo<CR>")
map("n", "rd", "<cmd>RustOpenExternalDocs<CR>")
-- Rust Analyzer

-- map("n", "ro", "<cmd>CocCommand rust-analyzer.openDocs<CR>", { noremap = true})
-- map("n", "rt", "<cmd>CocCommand rust-analyzer.openCargoToml<CR>", { noremap = true})
-- map("n", "rr", "<cmd>CocCommand rust-analyzer.run<CR>", { noremap = true})
-- map("n", "re", "<cmd>CocCommand rust-analyzer.reload<CR>", { noremap = true})
-- map("n", "rd", "<cmd>CocCommand rust-analyzer.debug<CR>", { noremap = true})
-- map("n", "re", "<cmd>CocCommand rust-analyzer.reload<CR>", { noremap = true})

--Telescope
map("n", "<leader>ff", ":Telescope find_files<CR>")
map("n", "<leader>fg", ":Telescope live_grep<CR>")
map("n", "<leader>fh", ":Telescope help_tags<CR>")
map("n", "<leader>fb", ":Telescope buffers<CR>")

-- Hop
map("", "s", ":HopChar2<cr>", { silent = true })
map("", "S", ":HopWord<cr>", { silent = true })

-- Terminal Toggling
-- local Terminal = require("toggleterm.terminal").Terminal
-- local floatTerminal = Terminal:new({
-- 	direction = "float",
-- })
--
-- map("n", "<leader>f", "<cmd>floatTerminal()<CR>", { noremap = true })
