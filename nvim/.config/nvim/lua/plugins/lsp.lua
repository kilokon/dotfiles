-- Reserve a space in the gutter
-- This will avoid an annoying layout shift in the screen
vim.diagnostic.config({
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "✘",
      [vim.diagnostic.severity.WARN] = "▲",
      [vim.diagnostic.severity.HINT] = "⚑",
      [vim.diagnostic.severity.INFO] = "»",
    },
  },
})
vim.opt.signcolumn = "yes"

-- Add cmp_nvim_lsp capabilities settings to lspconfig
-- This should be executed before you configure any language server
local lspconfig_defaults = require("lspconfig").util.default_config
lspconfig_defaults.capabilities = vim.tbl_deep_extend(
  "force",
  lspconfig_defaults.capabilities,
  require("cmp_nvim_lsp").default_capabilities()
)

-- This is where you enable features that only work
-- if there is a language server active in the file
vim.api.nvim_create_autocmd("LspAttach", {
  desc = "LSP actions",
  callback = function(event)
    local opts = { buffer = event.buf }

    vim.keymap.set("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)
    vim.keymap.set("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", opts)
    vim.keymap.set("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<cr>", opts)
    vim.keymap.set("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<cr>", opts)
    vim.keymap.set("n", "go", "<cmd>lua vim.lsp.buf.type_definition()<cr>", opts)
    vim.keymap.set("n", "gr", "<cmd>lua vim.lsp.buf.references()<cr>", opts)
    vim.keymap.set("n", "gs", "<cmd>lua vim.lsp.buf.signature_help()<cr>", opts)
    vim.keymap.set("n", "<F2>", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
    vim.keymap.set({ "n", "x" }, "<F3>", "<cmd>lua vim.lsp.buf.format({async = true})<cr>", opts)
    vim.keymap.set("n", "<F4>", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
  end,
})

for _, method in ipairs({ "textDocument/diagnostic", "workspace/diagnostic" }) do
  local default_diagnostic_handler = vim.lsp.handlers[method]
  vim.lsp.handlers[method] = function(err, result, context, config)
    if err ~= nil and err.code == -32802 then
      return
    end
    return default_diagnostic_handler(err, result, context, config)
  end
end

-- local lspconfig = require("lspconfig")

require("mason").setup({})
local lsp_zero = require("lsp-zero")

lsp_zero.extend_lspconfig()

require("mason-lspconfig").setup({
  -- Replace the language servers listed here
  -- with the ones you want to install
  ensure_installed = { "lua_ls", "jsonls", "taplo" },
  automatic_installation = false,
  handlers = {
    lsp_zero.default_setup,
    --
    --
    --
    --
    lua_ls = function()
      local lua_opts = lsp_zero.nvim_lua_ls()
      require("lspconfig").lua_ls.setup({
        lua_opts,
        root_dir = function()
          --- either .luarc.json or .stylua.toml
          return lsp_zero.dir.find_first({ ".luarc.json", ".stylua.toml" })
        end,
        on_attach = function(client, bufnr)
          -- on_attach,
        end,
        settings = {
          Lua = {
            completion = {
              callSnippet = "Replace",
            },
            hint = {
              enable = true,
            },
            diagnostics = {
              globals = { "vim" },
            },
            disable = {
              "luadoc-miss-see-name",
              "undefined-field",
            },
            runtime = {
              version = "LuaJIT",
            },
            workspace = {
              library = {
                vim.env.VIMRUNTIME .. "/lua",
              },
              checkThirdParty = "Disable",
            },
          },
        },
      })
    end,
    --
    --
    --
    --
    rust_analyzer = lsp_zero.noop,
  },
})
vim.g.rustaceanvim = {
  server = {
    capabilities = require("cmp_nvim_lsp").default_capabilities(),
  },
}

require("luasnip.loaders.from_vscode").lazy_load({
  paths = {
    vim.fn.stdpath("data") .. "/lazy/friendly-snippets",
    vim.fn.stdpath("config") .. "/snippets",
  },
})

local cmp = require("cmp")
local luasnip = require("luasnip")

local select_opts = { behavior = cmp.SelectBehavior.Select }

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  sources = {
    { name = "path" },
    { name = "nvim_lsp", keyword_length = 1 },
    { name = "buffer", keyword_length = 3 },
    { name = "luasnip", keyword_length = 2 },
  },
  window = {
    documentation = cmp.config.window.bordered(),
  },
  formatting = {
    expandable_indicator = true,
    fields = { "menu", "abbr", "kind" },
    format = function(entry, item)
      local menu_icon = {
        nvim_lsp = "λ",
        luasnip = "⋗",
        buffer = "Ω",
        path = "🖫",
      }

      item.menu = menu_icon[entry.source.name]
      return item
    end,
  },
  mapping = {
    ["<CR>"] = cmp.mapping.confirm({ select = false }),
    ["<Tab>"] = cmp.mapping(function(fallback)
      local col = vim.fn.col(".") - 1

      if cmp.visible() then
        cmp.select_next_item(select_opts)
      elseif col == 0 or vim.fn.getline("."):sub(col, col):match("%s") then
        fallback()
      else
        cmp.complete()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item(select_opts)
      else
        fallback()
      end
    end, { "i", "s" }),
  },
})
