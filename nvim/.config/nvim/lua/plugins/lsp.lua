-- handlers["textDocument/hover"] = vim.lsp.with(handlers.hover, popup_opts)
-- handlers["textDocument/signatureHelp"] = vim.lsp.with(handlers.signature_help, popup_opts)

---@diagnostic disable: missing-fields
return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "b0o/SchemaStore.nvim",
      "Hoffs/omnisharp-extended-lsp.nvim",
      -- { "saghen/blink.cmp" },
    },
    -- opts = {
    --   diagnostics = {
    --     update_in_insert = true,
    --     virtual_text = {
    --       prefix = "■",
    --       source = true,
    --     },
    --   },
    --   inlay_hints = {
    --     enabled = true,
    --   },
    --   codelens = {
    --     enabled = true,
    --   },
    -- },
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      local lspconfig = require("lspconfig")
      local util = require("lspconfig.util")
      local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

      vim.api.nvim_create_autocmd({ "CursorHold" }, {
        pattern = "*",
        callback = function()
          for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
            if vim.api.nvim_win_get_config(winid).zindex then
              return
            end
          end
          vim.diagnostic.open_float({
            scope = "cursor",
            focusable = false,
            -- Need to find event of where line/word/character under cursor is deleted.
            close_events = {
              "CursorMoved",
              "CursorMovedI",
              "BufHidden",
              "InsertCharPre",
              "WinLeave",
            },
          })
        end,
      })
      -- local handlers = {
      --   -- ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      --   --   -- Disable virtual_text
      --   --   underline = true,
      --   --   virtual_text = {
      --   --     spacing = 5,
      --   --     -- severity_limit = "Warning",
      --   --   },
      --   --   update_in_insert = true,
      --   --   -- virtual_text = false,
      --   -- }),
      --   ["eslint/openDoc"] = function(_, result)
      --     if result then
      --       vim.ui.open(result.url)
      --     end
      --     return {}
      --   end,
      --   ["eslint/confirmESLintExecution"] = function(_, result)
      --     if not result then
      --       return
      --     end
      --     return 4 -- approved
      --   end,
      --   ["eslint/probeFailed"] = function()
      --     vim.notify("[lspconfig] ESLint probe failed.", vim.log.levels.WARN)
      --     return {}
      --   end,
      --   ["eslint/noLibrary"] = function()
      --     vim.notify("[lspconfig] Unable to find ESLint library.", vim.log.levels.WARN)
      --     return {}
      --   end,
      -- }
      -- vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      --   -- underline = true,
      --   virtual_text = {
      --     spacing = 5,
      --     severity_limit = "Warning",
      --   },
      --   update_in_insert = true,
      -- })
      ---@diagnostic disable-next-line: unused-local
      local function on_attach(client, bufnr)
        -- Disable semantic highlights
        -- client.server_capabilities.semanticTokensProvider = nil
        -- Disable formatting capabilities ( Using cli commands to format )
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentFormattingRangeProvider = false
        -- vim.lsp.inlay_hint.enable(true, { bufnr })
      end

      local servers = {
        "lua_ls",
        "nushell",
        "pyright",
        "clangd",
        "taplo",
        "cmake",
        "jsonls",
        "yamlls",
        "nushell",
        "c3_lsp",
        "glsl_analyzer",
        "tinymist",
        "ts_ls",
        -- "eslint",
      }
      lspconfig.bashls.setup({
        cmd = { "bash-language-server", "start" },
        cmd_env = {
          GLOB_PATTERN = "*@(.sh|.inc|.bash|.command)",
        },
        filetypes = { "sh" },
        root_dir = function(path)
          if not path or #path == 0 then
            return
          end
          local result = path:gsub(strip_sep_pat, ""):gsub(strip_dir_pat, "")
          if #result == 0 then
            return "/"
          end
          return result
        end,
      })
      lspconfig.c3_lsp.setup({
        cmd = { "c3lsp" },
        filetypes = { "c3", "c3i" },
        root_dir = function(fname)
          return util.root_pattern({ "project.json", "manifest.json", ".git" })(fname)
        end,
      })

      lspconfig.cssls.setup({
        cmd = { "vscode-css-language-server", "--stdio" },
        filetypes = { "css", "scss", "less" },
        root_markers = { "package.json", ".git" },
        settings = {
          css = {
            validate = true,
          },
          less = {
            validate = true,
          },
          scss = {
            validate = true,
          },
        },
      })

      lspconfig.clangd.setup({
        cmd = { "clangd" },
        root_markers = { ".clangd", ".clang-format", "compile_commands.json", "compile_flags.txt" },
        filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },
        on_init = function(client, result)
          if result.offsetEncoding then
            client.offset_encoding = result.offsetEncoding
          end
        end,
        root_dir = function(fname)
          local filename = util.path.is_absolute(fname) and fname or table.concat({ vim.fn.getcwd(), fname })
          return root_pattern(filename) or vim.fs.dirname
        end,
      })

      lspconfig.cmake.setup({
        cmd = { "cmake-language-server" },
        filetypes = { "cmake" },
        init_options = {
          buildDirectory = "build",
        },
        root_dir = function(fname)
          return util.root_pattern(".git", "compile_commands.json", "build")(fname) or vim.fs.dirname
        end,
        on_new_config = function(new_config, new_cwd)
          local status, cmake = pcall(require, "cmake-tools")
          if status then
            cmake.clangd_on_new_config(new_config)
          end
        end,
      })

      lspconfig.emmet_language_server.setup({
        filetypes = {
          "css",
          "eruby",
          "html",
          "javascript",
          "javascriptreact",
          "less",
          "sass",
          "scss",
          "pug",
          "typescriptreact",
        },
        init_options = {
          includeLanguages = {},
          excludeLanguages = {},
          extensionsPath = {},
          preferences = {},
          showAbbreviationSuggestions = true,
          showExpandedAbbreviation = "always",
          showSuggestionsAsSnippets = false,
          syntaxProfiles = {},
          variables = {},
        },
      })

      lspconfig.eslint.setup({
        on_attach = function(client, bufnr)
          vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = bufnr,
            command = "EslintFixAll",
          })
        end,
      })

      -- GDScript
      local port = os.getenv("GDScript_Port") or "6005"
      local cmd = vim.lsp.rpc.connect("127.0.0.1", tonumber(port))

      lspconfig["gdscript"].setup({
        cmd = cmd,
        filetypes = { "gd", "gdscript", "gdscript3" },
        root_dir = util.root_pattern("project.godot", ".git"),
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

      lspconfig.glsl_analyzer.setup({
        cmd = { "glsl_analyzer" },
        filetypes = { "glsl", "vert", "tesc", "tese", "frag", "geom", "comp" },
        root_dir = function(fname)
          return vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1])
        end,
        single_file_support = true,
        --   docs = {
        --     description = [[
        --       https://github.com/nolanderc/glsl_analyzer
        --       Language server for GLSL
        --     ]],
        -- },
        -- capabilities = {},
      })

      lspconfig.lemminx.setup({
        cmd = { "lemminx" },
        filetypes = { "xml", "xsd", "xsl", "xslt", "svg" },
        root_dir = function(fname)
          return vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1])
        end,
        single_file_support = true,
      })

      lspconfig.lua_ls.setup({
        cmd = { "lua-language-server" },
        filetypes = { "lua" },
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

      lspconfig.marksman.setup({
        cmd = { "marksman", "server" },
        filetypes = { "markdown", "markdown.mdx" },
        root_dir = function(fname)
          local root_files = { ".marksman.toml" }
          return util.root_pattern(unpack(root_files))(fname)
            or vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1])
        end,
        single_file_support = true,
      })

      lspconfig.markdown_oxide.setup({
        cmd = { "markdown-oxide" },
        root_dir = function(fname, _)
          return require("lspconfig").util.root_pattern(".obsidian", ".moxide.toml", ".git")(fname) or vim.uv.cwd()
        end,
        capabilities = vim.tbl_deep_extend("force", capabilities, {
          workspace = {
            didChangeWatchedFiles = {
              dynamicRegistration = true,
            },
          },
        }),
        commands = {
          -- Today = {
          --   function()
          --     vim.lsp.buf.execute_command({ command = "jump", arguments = { "today" } })
          --   end,
          --   description = "Open today's daily note",
          -- },
          -- Tomorrow = {
          --   function()
          --     vim.lsp.buf.execute_command({ command = "jump", arguments = { "tomorrow" } })
          --   end,
          --   description = "Open tomorrow's daily note",
          -- },
          -- Yesterday = {
          --   function()
          --     vim.lsp.buf.execute_command({ command = "jump", arguments = { "yesterday" } })
          --   end,
          --   description = "Open yesterday's daily note",
          -- },
        },
      })

      lspconfig.nushell.setup({
        cmd = { "nu", "--lsp" },
        filetypes = { "nu" },
        root_dir = function(fname)
          return vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1] or fname)
        end,
        single_file_support = true,
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

      -- lspconfig.rustowl.setup({
      --   trigger = {
      --     hover = false,
      --   },
      -- })

      lspconfig.taplo.setup({
        cmd = { "taplo", "lsp", "stdio" },
        filetypes = { "toml" },
        single_file_support = true,
      })

      -- TypeScript
      lspconfig.ts_ls.setup({
        enabled = false,
        -- on_attach = on_attach,
        init_options = { hostInfo = "neovim" },
        filetypes = {
          "javascript",
          "javascriptreact",
          "javascript.jsx",
          "typescript",
          "typescriptreact",
          "typescript.tsx",
        },
        cmd = { "typescript-language-server", "--stdio" },
        root_dir = util.root_pattern("tsconfig.json", "jsconfig.json", "package.json", ".git"),
        single_file_support = true,
        -- root_dir = nvim_lsp.util.root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git"),
        commands = {
          OrganizeImports = {
            organize_imports,
            description = "Organize Imports",
          },
        },
      })

      lspconfig.tinymist.setup({
        -- https://github.com/Myriad-Dreamin/tinymist
        cmd = { "tinymist" },
        filetypes = { "typst" },
        root_dir = function(fname)
          return vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1])
        end,
        single_file_support = true,
      })

      lspconfig.yamlls.setup({
        cmd = { "yaml-language-server", "--stdio" },
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
      -- Lsp Diagnostics
      local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
      end
      vim.diagnostic.config({
        severity_sort = true,

        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = "✘",
            [vim.diagnostic.severity.WARN] = "▲",
            [vim.diagnostic.severity.HINT] = "⚑",
            [vim.diagnostic.severity.INFO] = "»",
          },
        },
      })
      for _, lsp in ipairs(servers) do
        lspconfig[lsp].setup({
          on_attach = on_attach,
          capabilities = capabilities,
          -- handlers = handlers,
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

      local glance = require("glance")
      glance.setup({
        theme = {
          enable = true,
          mode = "darken",
        },
      })
    end,
  },
  {
    "aznhe21/actions-preview.nvim",
    event = "LspAttach",
    -- keys = {
    --   { "gf", "<cmd>require('actions-preview').code_actions<CR>", desc = "Action Preview" },
    -- },
    config = function()
      require("actions-preview").setup({
        vim.keymap.set({ "v", "n" }, "<F4>", require("actions-preview").code_actions),
      })
    end,
  },
  {
    "jmbuhr/otter.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      vim.api.nvim_create_autocmd({ "FileType" }, {
        pattern = { "toml" },
        group = vim.api.nvim_create_augroup("EmbedToml", {}),
        callback = function()
          require("otter").activate()
        end,
      })
    end,
  },
  {
    "ehaynes99/nvim-eslint",
    config = function()
      require("nvim-eslint").setup({})
    end,
  },
}
