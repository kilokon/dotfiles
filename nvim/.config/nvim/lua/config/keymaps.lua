local map = vim.keymap.set
local keymap = vim.api.nvim_buf_set_keymap
local M = {}
vim.api.nvim_create_user_command("W", "w", { bang = true })
vim.api.nvim_create_user_command("Wq", "wq", { bang = true })
vim.api.nvim_create_user_command("WQ", "wq", { bang = true })

-- 'W': The command you’re aliasing (case-sensitive, so :W triggers it).
-- 'w': The command it executes (writes the buffer to disk).
-- bang = true: Allows :W! to work as :w! (force write).

map("n", "<esc>", "<cmd>nohlsearch<cr><esc>")

map({ "i", "x", "n", "s" }, "<C-s>", "<cmd>update<cr><esc>", { desc = "Save file" })
vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save file" })

-- Remap for dealing with word wrap
map({ "n", "v" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
map({ "n", "v" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- better indenting
map("v", "<", "<gv", { desc = "Indent left" })
map("v", ">", ">gv", { desc = "Indent right" })

-- Buffer Surfing
map("n", "]b", "<cmd>bn<cr>", {})
map("n", "[b", "<cmd>bp<cr>", {})
-- map("n")

-- disable right click
map({ "n", "x", "i", "v", "o" }, "<RightMouse>", "<Nop>")
map({ "n", "x", "i", "v", "o" }, "<2-RightMouse>", "<Nop>")
map({ "n", "x", "i", "v", "o" }, "<3-RightMouse>", "<Nop>")
map({ "n", "x", "i", "v", "o" }, "<4-RightMouse>", "<Nop>")
-- disable middle mouse click paste
map({ "n", "x", "i", "v", "o" }, "<MiddleMouse>", "<Nop>")
map({ "n", "x", "i", "v", "o" }, "<2-MiddleMouse>", "<Nop>")
map({ "n", "x", "i", "v", "o" }, "<3-MiddleMouse>", "<Nop>")
map({ "n", "x", "i", "v", "o" }, "<4-MiddleMouse>", "<Nop>")

vim.api.nvim_set_keymap("i", "jj", "<Esc>", { noremap = false })
vim.api.nvim_set_keymap("i", "jk", "<Esc>", { noremap = false })
-- Ignite Lazy
vim.keymap.set("n", "<leader>l", "<cmd>Lazy<cr>", { desc = "Lazy" })

-- Escape from terminal
-- map("t", "<Esc>", [[<C-\><C-n>]], { noremap = true, expr = true })
-- vim.cmd([[tnoremap <Esc> <C-\><C-n>]])
-- dap
vim.keymap.set("n", "<leader><space>", function()
  require("dap").toggle_breakpoint()
end)
vim.keymap.set("n", "<F5>", function()
  require("dap").continue()
end)
vim.keymap.set("n", "<leader>dui", function()
  require("dapui").toggle()
end)
vim.keymap.set("n", "<leader>si", function()
  require("dap").step_into()
end)
vim.keymap.set("n", "<leader>sv", function()
  require("dap").step_over()
end)
vim.keymap.set("n", "<leader>so", function()
  require("dap").step_over()
end)
vim.keymap.set("n", "<Leader>dr", function()
  require("dap").repl.open()
end)
vim.keymap.set("n", "<leader>dt", function()
  require("dap").terminate()
end)

-- Glance

-- rustaceanvim
vim.keymap.set("n", "<leader>rd", ":RustLsp debuggables<CR>")
vim.keymap.set("n", "<leader>rr", ":RustLsp runnables<CR>")

local function lsp_keymaps(bufnr)
  local opts = { noremap = true, silent = true }
  keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  -- keymap(bufnr, "n", "gI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  -- keymap(bufnr, "n", "gl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
  -- keymap(bufnr, "n", "<leader>lf", "<cmd>lua vim.lsp.buf.format{ async = true }<cr>", opts)
  keymap(bufnr, "n", "<leader>li", "<cmd>LspInfo<cr>", opts)
  keymap(bufnr, "n", "<leader>lI", "<cmd>LspInstallInfo<cr>", opts)
  keymap(bufnr, "n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
  keymap(bufnr, "n", "<leader>lj", "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>", opts)
  keymap(bufnr, "n", "<leader>lk", "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>", opts)
  keymap(bufnr, "n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
  keymap(bufnr, "n", "<leader>ls", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  keymap(bufnr, "n", "<leader>lq", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
end

-- For Vortex Keyboard
vim.keymap.set("i", "<S-Esc>", "<Nop>", { silent = true })

M.on_attach = function(client, bufnr)
  lsp_keymaps(bufnr)
end

return M
