---@diagnostic disable: missing-fields
return {
  {
    "neovim/nvim-lspconfig",
    dependencies = { "hrsh7th/cmp-nvim-lsp", "b0o/SchemaStore.nvim" },
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      local lspconfig = require("lspconfig")
      -- local actions = require("lsp.actions")
      -- local lsp_keys = function(_, bufnr)
      --   if vim.b.large_buf then
      --     return
      --   end
      --
      --   -- Mappings.
      --   local map = function(keys, func, desc)
      --     vim.keymap.set("n", keys, func, { buffer = bufnr, desc = "LSP: " .. desc, nowait = true })
      --   end
      --
      --   -- Code Actions
      --   map("<leader>cr", vim.lsp.buf.rename, "Code Rename")
      --   map("<leader>ca", vim.lsp.buf.code_action, "Code Action")
      --   map("<leader>cc", vim.lsp.codelens.run, "Run Codelens")
      --   -- Hover Actions
      --   map("K", vim.lsp.buf.hover, "Hover Documentation")
      --   -- LSP/Glance Actions
      --   map("gD", "<CMD>Glance definitions<CR>", "Glance Definitions")
      --   map("gR", "<CMD>Glance references<CR>", "Glance References")
      --   map("gY", "<CMD>Glance type_definitions<CR>", "Type Definitions")
      --   map("gM", "<CMD>Glance type_implementations<CR>", "Type Implementations")

      -- vim.keymap.set("n", "<C-x>", vim.lsp.buf.code_action, bufnr)
      -- vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, bufopts)
      -- vim.keymap.set("n", "gh", vim.lsp.buf.hover, bufopts)
      -- -- Open definition in a split window
      -- vim.keymap.set("n", "<C-w>gd", function()
      --   local params = vim.lsp.util.make_position_params()
      --   return vim.lsp.buf_request(0, "textDocument/definition", params, actions.goto_definition())
      -- end, bufopts)
      -- vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
      -- vim.keymap.set("n", "gl", vim.lsp.buf.declaration, bufopts)
      -- vim.keymap.set("n", "gm", vim.lsp.buf.implementation, bufopts)
      -- vim.keymap.set("n", "gy", vim.lsp.buf.type_definition, bufopts)
      -- vim.keymap.set("n", "gr", "<CMD>Glance references<CR>", bufopts)
      -- vim.keymap.set("n", "gD", "<CMD>Glance definitions<CR>", bufopts)
      -- vim.keymap.set("n", "gY", "<CMD>Glance type_definitions<CR>", bufopts)
      -- vim.keymap.set("n", "gM", "<CMD>Glance implementations<CR>", bufopts)
      -- vim.keymap.set("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, bufopts)
      -- vim.keymap.set("n", "[d", vim.diagnostic.goto_prev)
      -- vim.keymap.set("n", "]d", vim.diagnostic.goto_next)
      -- vim.keymap.set("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
      -- vim.keymap.set("n", "<leader>wl", function()
      --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      -- end, bufopts)
      --
      -- -- Symbols
      -- vim.keymap.set("n", "<leader>sw", "<CMD>Telescope lsp_workspace_symbols<CR>", bufopts)
      -- vim.keymap.set("n", "<leader>sb", "<CMD>Telescope lsp_document_symbols<CR>", bufopts)
      -- vim.keymap.set("n", "ge", vim.lsp.buf.rename, bufopts)
      -- end

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require("cmp_nvim_lsp").default_capabilities()
      local function on_attach(client, bufnr)
        vim.lsp.inlay_hint.enable(true, { bufnr })
        -- require("config.keymaps").on_attach(client, bufnr)
        -- lsp_keys(client, bufnr)
      end
      local handlers = {
        ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
          silent = true,
          -- border = EcoVim.ui.float.border,
        }),
      }
      local servers = {
        "lua_ls",
        "nushell",
        "pyright",
        "clangd",
        -- "rust_analyzer",
      }
      lspconfig.lua_ls.setup({
        runtime = {
          version = "LuaJIT",
        },
        workspace = {
          checkThirdParty = false,
          library = {
            -- vim.env.VIMRUNTIME,
            [vim.fn.expand("$VIMRUNTIME/lua")] = true,
            [vim.fn.stdpath("config") .. "/lua"] = true,
          },
        },
        codeLens = {
          enable = true,
        },
        hint = {
          enable = true,
          setType = false,
          paramType = true,
          paramName = "Disable",
          semicolon = "Disable",
          arrayIndex = "Disable",
        },
      })
      lspconfig.clangd.setup({
        cmd = { "clangd" },
        root_markers = { ".clangd", "compile_commands.json" },
        filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },
      })

      lspconfig.jsonls.setup({
        settings = {
          json = {
            schemas = require("schemastore").json.schemas(),
            validate = {
              enable = true,
            },
          },
        },
      })

      lspconfig.yamlls.setup({
        settings = {
          yaml = {
            schemas = require("schemastore").yaml.schemas(),
            validate = {
              enable = true,
            },
            keyOrdering = false,
          },
        },
      })

      lspconfig.pyright.setup({
        settings = {
          python = {
            analysis = {
              typeCheckingMode = "off",
            },
          },
        },
      })

      lspconfig.nushell.setup({
        cmd = { "nu", "--lsp" },
        filetypes = { "nu" },
        -- root_dir = require("lspconfig.util").find_git_ancestor,
        single_file_support = true,
      })

      for _, lsp in ipairs(servers) do
        lspconfig[lsp].setup({
          on_attach = on_attach,
          capabilities = capabilities,
          handlers = handlers,
        })
      end
    end,
  },
  {
    "dnlhc/glance.nvim",
    -- cmd = "Glance",
    event = "LspAttach",
    config = function()
      vim.keymap.set("n", "gD", "<CMD>Glance definitions<CR>")
      vim.keymap.set("n", "gR", "<CMD>Glance references<CR>")
      vim.keymap.set("n", "gY", "<CMD>Glance type_definitions<CR>")
      vim.keymap.set("n", "gM", "<CMD>Glance implementations<CR>")
      --       vim.keymap.set("n", "gD", "<CMD>Glance definitions<CR>")
      -- vim.keymap.set("n", "gR", "<CMD>Glance references<CR>")
      -- vim.keymap.set("n", "gY", "<CMD>Glance type_definitions<CR>")
      -- vim.keymap.set("n", "gM", "<CMD>Glance implementations<CR>")
      local glance = require("glance")
      glance.setup({
        theme = {
          enable = true,
          mode = "darken",
        },
      })
    end,
  },
}
