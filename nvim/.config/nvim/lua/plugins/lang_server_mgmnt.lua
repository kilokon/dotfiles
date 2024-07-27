return {
  {
    "VonHeikemen/lsp-zero.nvim",
    branch = "v4.x",
    lazy = true,
    config = false,
    init = function()
      -- Disable automatic setup, we are doing it manually
      -- vim.g.lsp_zero_extend_cmp = 0
      -- vim.g.lsp_zero_extend_lspconfig = 0
      -- vim.b.lsp_zero_enable_autoformat = 1
    end,
  },
  {
    "SmiteshP/nvim-navic",
    config = function()
      require("nvim-navic").setup({
        icons = {
          File = " ",
          Module = " ",
          Namespace = " ",
          Package = " ",
          Class = " ",
          Method = " ",
          Property = " ",
          Field = " ",
          Constructor = " ",
          Enum = "練",
          Interface = "練",
          Function = " ",
          Variable = " ",
          Constant = " ",
          String = " ",
          Number = " ",
          Boolean = "◩ ",
          Array = " ",
          Object = " ",
          Key = " ",
          Null = "ﳠ ",
          EnumMember = " ",
          Struct = " ",
          Event = " ",
          Operator = " ",
          TypeParameter = " ",
        },
        highlight = true,
      })
    end,
  },
  {
    "williamboman/mason.nvim",
    cmd = { "Mason", "MasonInstall", "MasonUpdate" },
    lazy = true,
    opts = {
      ensure_installed = {},
      ui = {
        border = "rounded",
      },
    },
    config = true,
  },
  { "williamboman/mason-lspconfig.nvim", enabled = true },
  -- { "simrat39/inlay-hints.nvim" },
  { "hrsh7th/cmp-nvim-lsp" },
  { "lukas-reineke/lsp-format.nvim" },
  { "onsails/lspkind.nvim" },
  { "b0o/schemastore.nvim" },

  -- { "simrat39/rust-tools.nvim" },
  {
    "dnlhc/glance.nvim",
    keys = {
      { "gD", "<CMD>Glance definitions<CR>" },
      { "gR", "<CMD>Glance references<CR>" },
      { "gY", "<CMD>Glance type_definitions<CR>" },
      { "gM", "<CMD>Glance implementations<CR>" },
    },
  },


  { "lvimuser/lsp-inlayhints.nvim" },

  {
    "neovim/nvim-lspconfig",
    cmd = { "LspInfo", "LspInstall", "LspStart" },
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
      vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)
      -- This is where all the LSP shenanigans will live

      local lsp_zero = require("lsp-zero")
      lsp_zero.extend_lspconfig()
      -- local ih = require("inlay-hints")
      local ih = require("lsp-inlayhints")
      ih.setup()

      lsp_zero.on_attach(function(client, bufnr)
        -- see :help lsp-zero-keybindings
        -- to learn the available actions
        lsp_zero.default_keymaps({
          buffer = bufnr,
          exclude = { "gD", "gR", "gY", "gM" }, -- Glance will handle these
        })

        if client.server_capabilities.documentSymbolProvider then
          require("nvim-navic").attach(client, bufnr)
        end
      end)

      lsp_zero.set_server_config({
        capabilities = {
          textDocument = {
            foldingRange = {
              dynamicRegistration = false,
              lineFoldingOnly = true,
            },
          },
        },
      })

      vim.g.rustaceanvim = {

        tools = {
          hover_actions = {
            auto_focus = true,
          },
        },
        server = {
          on_attach = function(client, bufnr)
            ih.on_attach(client, bufnr)
            -- require("lsp-inlayhints").on_attach(client, bufnr)
            local opts = { noremap = true, silent = true }
            vim.api.nvim_buf_set_keymap(
              bufnr,
              "n",
              "s",
              '<cmd>lua require("tree_climber_rust").init_selection()<CR>',
              opts
            )
            vim.api.nvim_buf_set_keymap(
              bufnr,
              "x",
              "s",
              '<cmd>lua require("tree_climber_rust").select_incremental()<CR>',
              opts
            )
            vim.api.nvim_buf_set_keymap(
              bufnr,
              "x",
              "S",
              '<cmd>lua require("tree_climber_rust").select_previous()<CR>',
              opts
            )
            -- you can also put keymaps in here
          end,
          inlayHints = {
            bindingModeHints = {
              enable = false,
            },
            chainingHints = {
              enable = true,
            },
            closingBraceHints = {
              enable = true,
              minLines = 25,
            },
            closureReturnTypeHints = {
              enable = "never",
            },
            lifetimeElisionHints = {
              enable = "never",
              useParameterNames = false,
            },
            maxLength = 25,
            parameterHints = {
              enable = true,
            },
            reborrowHints = {
              enable = "never",
            },
            renderColons = true,
            typeHints = {
              enable = true,
              hideClosureInitialization = false,
              hideNamedConstructor = false,
            },
          },
          -- capabilities = lsp_zero.get_capabilities(),
        },
      }

      lsp_zero.set_sign_icons({
        error = "✘",
        warn = "▲",
        hint = "⚑",
        info = "»",
      })
      require("mason-lspconfig").setup({
        ensure_installed = {
          "clangd",
          "lua_ls",
          "cmake",
          "neocmake",
          "powershell_es",
          "pyright",
          "ruff_lsp",
          "rust_analyzer",
          "taplo",
          "typst_lsp",
          "yamlls",
        },
        handlers = {
          lsp_zero.default_setup,


          --LUA LSP
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
                ih.on_attach(client, bufnr)
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

          -- JSONLS
          jsonls = function()
            require("lspconfig").jsonls.setup({
              settings = {
                json = {
                  schemas = require("schemastore").json.schemas(),
                  validate = { enable = true },
                },
              },
            })
          end,

          -- POWERSHELL
          powershell_es = function()
            require("lspconfig").powershell_es.setup({
              cmd = {
                "pwsh",
                "-NoLogo",
                "-NoProfile",
                "-Command ",
                "/opt/powershell-editor-services/PowerShellEditorServices/Start-EditorServices.ps1",
                "-SessionDetailsPath",
                ".session.json",
              },
              bundle_path = "/opt/powershell-editor-services",
              root_dir = function()
                -- return util.find_git_ancestor(fname) or vim.fn.getcwd()
                return lsp_zero.dir.find_first({ ".git" }) or vim.fn.getcwd()
              end,
            })
          end,

          -- Python Ruff Linter and Formatter
          -- ruff_lsp = function()
          --   require("lspconfig").ruff_lsp.setup({
          --     init_options = {
          --       settings = {
          --         -- Any extra CLI arguments for `ruff` go here.
          --         args = {},
          --       },
          --     },
          --   })
          -- end,

          -- C/C++ LSP
          clangd = function()
            require("lspconfig").clangd.setup({
              cmd = {
                "clangd",
                "--background-index",
                "--clang-tidy",
                "--completion-style=bundled",
                "--header-insertion=iwyu",
              },
              filetypes = { "c", "cpp", "objc", "objcpp", "h", "hpp" },
              on_attach = function(_, buf)
                local group = vim.api.nvim_create_augroup("clangd_no_inlay_hints_in_insert", { clear = true })

                vim.keymap.set("n", "<leader>lh", function()
                  if require("clangd_extensions.inlay_hints").toggle_inlay_hints() then
                    vim.api.nvim_create_autocmd("InsertEnter", {
                      group = group,
                      buffer = buf,
                      callback = require("clangd_extensions.inlay_hints").disable_inlay_hints
                    })
                    vim.api.nvim_create_autocmd({ "TextChanged", "InsertLeave" }, {
                      group = group,
                      buffer = buf,
                      callback = require("clangd_extensions.inlay_hints").set_inlay_hints
                    })
                  else
                    vim.api.nvim_clear_autocmds({ group = group, buffer = buf })
                  end
                end, { buffer = buf, desc = "[l]sp [h]ints toggle" })
              end,
              -- on_attach = function(_, _)
              --   require("clangd_extensions.inlay_hints").setup_autocmd()
              --   require("clangd_extensions.inlay_hints").set_inlay_hints()
              --
              -- end,
            })
          end,

          -- Typst
          typst_lsp = function()
            require("lspconfig").typst_lsp.setup({
              settings = {
                exportPdf = "onType", -- Choose onType, onSave or never.
                -- serverPath = "" -- Normally, there is no need to uncomment it.
              },
            })
          end,

          rust_analyzer = lsp_zero.noop,
        },
      })
      lsp_zero.setup()
    end,
  },
  {
    "linux-cultist/venv-selector.nvim",
    dependencies = {
      "neovim/nvim-lspconfig",
      "mfussenegger/nvim-dap",
      "mfussenegger/nvim-dap-python", --optional
      { "nvim-telescope/telescope.nvim", branch = "0.1.x", dependencies = { "nvim-lua/plenary.nvim" } },
    },
    lazy = false,
    branch = "regexp", -- This is the regexp branch, use this for the new version
    config = function()
      require("venv-selector").setup()
    end,
    keys = {
      { ",v", "<cmd>VenvSelect<cr>" },
    },
  },
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require("copilot").setup({})
    end,
  },

  {
    "Civitasv/cmake-tools.nvim",
    ft = { "cmake" },
    dependencies = {
      "stevearc/overseer.nvim",
    },
    config = function()
      local osys = require("cmake-tools.osys")
      require("cmake-tools").setup({
        cmake_build_directory = function()
          if osys.iswin32 then
            return "out\\${variant:buildType}"
          end
          return "out/${variant:buildType}"
        end,
        -- cmake_build_directory = "build",
      })
    end,
  },
  {
    "mrcjkb/rustaceanvim",
    version = "^4", -- Recommended
    lazy = false,   -- This plugin is already lazy
    keys = {
      { "<leader>rh", ":lua vim.cmd.RustLsp { 'hover', 'actions' }" },
      { "<leader>rr", ":lua vim.cmd.RustLsp('run')" },
      { "<leader>rd", ":lua vim.cmd.RustLsp('debug')" },
      { "<leader>rc", ":lua vim.cmd.RustLsp('openCargo')" },
    }
  },
  --
  {
    "p00f/clangd_extensions.nvim",
    event = "VeryLazy",
    ft = { "c", "cpp", "h", "hpp" },
    config = function()
      require("clangd_extensions").setup({
        -- Setup
      })
    end,
  },

  { "NoahTheDuke/vim-just" },
  { "rafcamlet/nvim-luapad" },

  { -- This plugin
    "Zeioth/compiler.nvim",
    cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
    dependencies = { "stevearc/overseer.nvim", "nvim-telescope/telescope.nvim" },
    opts = {

      strategy = "toggleterm",
    },
    keys = {
      { "<leader>cc", "<cmd>CompilerOpen<CR>" },
      { "<leader>cs", "<cmd>CompilerStop<CR>" },
      { "<leader>ct", "<cmd>CompilerToggleResults<CR>" },
      { "<leader>cr", "<cmd>CompilerStop<CR><cmd>CompilerRedo<CR>" },
    },
  },
  { -- The task runner we use
    "stevearc/overseer.nvim",
    commit = "6271cab7ccc4ca840faa93f54440ffae3a3918bd",
    cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
    lazy = false,
    keys = {
      { "<leader>co", "<cmd>CompilerStop<CR><cmd>OverseerClose<CR>" },
    },
    opts = {
      strategy = "toggleterm",
      task_list = {
        direction = "side",
        -- min_height = 25,
        -- max_height = 25,
        default_detail = 1,
        bindings = {
          ["q"] = function()
            vim.cmd("OverseerClose")
          end,
        },
      },
    },
    config = function()
      require("overseer").setup({
        templates = { "builtin", "user.run_script", "user.cpp_build", "user.cc_just" },
      })
    end,
  },
  -- { "krady21/compiler-explorer.nvim" },
  {
    "j-hui/fidget.nvim",
    tag = "legacy",
    event = "LspAttach",
    config = function()
      require("fidget").setup()
    end,
  },

  {
    "saecki/crates.nvim",
    event = { "BufRead Cargo.toml" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("crates").setup({
        autoupdate_throttle = 50,
        max_parallel_requests = 32,
        popup = {
          -- border = shared.window.border,
          show_version_date = true,
        },
        completion = {
          crates = {
            enabled = true,
            max_results = 30,
          },
          cmp = {
            use_custom_kind = true,
          },
        },
        lsp = {
          enabled = true,
          -- on_attach = lsp_config.on_attach,
          actions = true,
          completion = true,
          hover = true,
        },
      })
    end,
  },
  {
    "lewis6991/hover.nvim",
    keys = {
      -- {'<MouseMove>', "<cmd> :lua require('hover').hover_mouse<CR>"}
    },
    config = function()
      vim.o.mousemoveevent = true
      require("hover").setup({
        init = function()
          -- Require providers
          require("hover.providers.lsp")
          -- require('hover.providers.gh')
          -- require('hover.providers.gh_user')
          -- require('hover.providers.jira')
          -- require('hover.providers.dap')
          -- require('hover.providers.fold_preview')
          require('hover.providers.diagnostic')
          -- require('hover.providers.man')
          -- require('hover.providers.dictionary')
        end,
      })
    end
  },
  {
    'kevinhwang91/nvim-ufo', dependencies = 'kevinhwang91/promise-async'
  },

  {
    "luukvbaal/statuscol.nvim",
    config = function()
      local builtin = require("statuscol.builtin")
      require("statuscol").setup({
        -- configuration goes here, for example:
        relculright = true,
        segments = {
          { text = { builtin.foldfunc }, click = "v:lua.ScFa" },
          {
            sign = { namespace = { "diagnostic/signs" }, maxwidth = 2, auto = true },
            click = "v:lua.ScSa"
          },
          { text = { builtin.lnumfunc }, click = "v:lua.ScLa", },
          {
            sign = { name = { ".*" }, maxwidth = 2, colwidth = 1, auto = true, wrap = true },
            click = "v:lua.ScSa"
          },
        },
        clickhandlers = { -- builtin click handlers
          Lnum                   = builtin.lnum_click,
          FoldClose              = builtin.foldclose_click,
          FoldOpen               = builtin.foldopen_click,
          FoldOther              = builtin.foldother_click,
          DapBreakpointRejected  = builtin.toggle_breakpoint,
          DapBreakpoint          = builtin.toggle_breakpoint,
          DapBreakpointCondition = builtin.toggle_breakpoint,
          ["diagnostic/signs"]   = builtin.diagnostic_click,
          gitsigns               = builtin.gitsigns_click,
        },
      })
    end,
  },
}
