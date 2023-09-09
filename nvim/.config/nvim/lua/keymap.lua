local function map(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local opts = { noremap = true, silent = true }
local opts_x = { expr = true, silent = true }
-- Leader and LocalLeader
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- new tab
map("n", "<leader><tab><tab>", "<cmd>tabnew<cr>", { desc = "New Tab" })

-- Cancel search highlighting with ESC
map("n", "<ESC>", ":nohlsearch<Bar>:echo<CR>", opts)

-- Visual --
-- Stay in indent mode
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

--smartquit
map("n", "qq", '<cmd>lua require("smart_quit")()<CR>')

-- Move down and up only one visual line
map("n", "j", "v:count==0 ? 'gj' : 'j'", opts_x)
map("n", "k", "v:count==0 ? 'gk' : 'k'", opts_x)

-- buffers
map(
  "n",
  "<leader>bb",
  "<cmd>lua require('telescope.builtin').buffers()<CR>",
  { desc = "Switcg Buffers" }
)

map(
  "n",
  "<C-s>",
  ':w<CR>:lua vim.notify(string.format("Saved %s", vim.fn.expand("%:t")), "info")<CR>',
  { silent = true }
)
map(
  "i",
  "<C-s>",
  '<esc>:w<CR>:lua vim.notify(string.format("Saved %s", vim.fn.expand("%:t")), "info")<CR>',
  { silent = true }
)
