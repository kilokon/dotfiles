local lsp_status_ok, lspconfig = pcall(require, "lspconfig")
if not lsp_status_ok then
  return
end

local status_ok, saga = pcall(require, "lspsaga")
if not status_ok then
  return
end

local util_status_ok, util = pcall(require, "lspconfig.util")
if not util_status_ok then
  return
end
-- Setup lspconfig.

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  local opts = { noremap = true, silent = true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
  buf_set_keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
  buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  buf_set_keymap(
    "n",
    "<C-k>",
    "<cmd>lua vim.lsp.buf.signature_help()<CR>",
    opts
  )
  buf_set_keymap(
    "n",
    "<space>D",
    "<cmd>lua vim.lsp.buf.type_definition()<CR>",
    opts
  )
  buf_set_keymap("n", "<space>r", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  buf_set_keymap(
    "n",
    "<space>a",
    "<cmd>lua vim.lsp.buf.code_action()<CR>",
    opts
  )
  buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  buf_set_keymap(
    "n",
    "<space>e",
    "<cmd>lua vim.diagnostic.open_float()<CR>",
    opts
  )
  buf_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  buf_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
  buf_set_keymap(
    "n",
    "<space>q",
    "<cmd>lua vim.diagnostic.set_loclist()<CR>",
    opts
  )
  buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

  -- Get signatures (and _only_ signatures) when in argument lists.
  require("lsp_signature").on_attach({
    doc_lines = 0,
    handler_opts = {
      border = "none",
    },
  })
end

local capabilities = require("cmp_nvim_lsp").update_capabilities(
  vim.lsp.protocol.make_client_capabilities()
)

-- LANGUAGE SERVERS
-- require("nvim-lsp-installer").setup({
-- 	ensure_installed = { "rust_analyzer" }, -- ensure these servers are always installed
-- 	automatic_installation = true, -- automatically detect which servers to install (based on which servers are set up via lspconfig)
-- 	ui = {
-- 		icons = {
-- 			server_installed = "✓",
-- 			server_pending = "➜",
-- 			server_uninstalled = "✗",
-- 		},
-- 	},
-- })

-- Fidget.nvim Standalone UI for nvim-lsp progress
require("fidget").setup({})
--
-- require("hover").setup{
--         init = function ()
--                 require('hover.providers.lsp')
--                 require('hover.providers.man')
--         end,
--         title = true
-- }
--
-- local root_files = {
--   "compile_commands.json",
--   ".ccls",
-- }
--
-- lspconfig.ccls.setup({
--   init_options = {
--     compilationDatabaseDirectory = "build",
--     cmd = { "ccls" },
--     filetypes = { "c", "cpp", "objc", "objcpp" },
--     index = {
--       threads = 0,
--     },
--     root_dir = function(fname)
--       return util.root_pattern(unpack(root_files))(fname)
--         or util.find_git_ancestor(fname)
--     end,
--     clang = {
--       excludeArgs = { "-frounding-math" },
--     },
--   },
-- })

-- lspconfig.ccls.setup({
--   init_options = {
--     compilationDatabaseDirectory = "build",
--     index = {
--       threads = 0,
--     },
--     clang = {
--       excludeArgs = { "-frounding-math" },
--     },
--   },
-- })
require("lspconfig").ccls.setup({})
-- Rust Language via Rust Analyzer
lspconfig.rust_analyzer.setup({
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  settings = {
    ["rust-analyzer"] = {
      cargo = {
        allFeatures = true,
      },
      completion = {
        postfix = {
          enable = false,
        },
      },
    },
  },
  capabilities = capabilities,
})

local luadev = require("lua-dev").setup({
  --add any options here, or leave empty to use the default settings
  lspconfig = {
    cmd = { "lua-language-server" },
  },
})
--Lua LSP
lspconfig.sumneko_lua.setup(luadev)

--Haskel Language Server
-- lspconfig.hls.setup({
-- 	cmd = { "haskell-language-server-wrapper", "--lsp" },
-- 	--filetypes = { "haskell", "lhaskell" },
-- 	--root_dir = root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"),
-- })

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics,
  {
    virtual_text = true,
    signs = true,
    update_in_insert = true,
  }
)

saga.init_lsp_saga()

--require'lspinstall'.setup() -- important

--require("lspconfig").graphql.setup {}
--END
