require("toggleterm").setup({
  open_mapping = [[<c-\>]],
})
local opts = {
  mode = "n", -- NORMAL mode
  prefix = "<leader>",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true, -- use `nowait` when creating keymaps
}

vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")

local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })

function _LAZYGIT_TOGGLE()
  lazygit:toggle()
end

local node = Terminal:new({ cmd = "node", hidden = true })

function _NODE_TOGGLE()
  node:toggle()
end

local python = Terminal:new({ cmd = "python3", hidden = true })

function _PYTHON_TOGGLE()
  python:toggle()
end

local wk = require("which-key")

local mappings = {
  { "<leader>tp", "<cmd>lua _PYTHON_TOGGLE()<cr>", desc = "Python Prompt" },
  { "<leader>tn", "<cmd>lua _NODE_TOGGLE()<cr>", desc = "Node Prompt" },
  { "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", desc = "Floating Terminal" },
}

wk.add(mappings, opts)
