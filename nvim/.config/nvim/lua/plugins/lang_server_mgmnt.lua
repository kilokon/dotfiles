return {
  {
    "VonHeikemen/lsp-zero.nvim",
    branch = "dev-v3",
    lazy = true,
    config = false,
    init = function()
      -- Disable automatic setup, we are doing it manually
      -- vim.g.lsp_zero_extend_cmp = 0
      -- vim.g.lsp_zero_extend_lspconfig = 0
      -- vim.b.lsp_zero_enable_autoformat = 0
    end,
  },
  {
    "L3MON4D3/LuaSnip",
    version = "2.*",
    dependencies = { "rafamadriz/friendly-snippets" },
    -- build = "make install_jsregexp",
    event = { "InsertEnter" },
    config = function()
      local ls = require("luasnip")
      local types = require("luasnip.util.types")

      ls.config.set_config({
        history = true,
        delete_check_events = "TextChanged",
        ext_opts = {
          [types.choiceNode] = {
            active = {
              virt_text = { { "choiceNode", "Comment" } },
            },
          },
        },
        -- treesitter-hl has 100, use something higher (default is 200).
        ext_base_prio = 300,
        -- minimal increase in priority.
        ext_prio_increase = 1,
        enable_autosnippets = true,
        -- mapping for cutting selected text so it's usable as SELECT_DEDENT,
        -- SELECT_RAW or TM_SELECTED_TEXT (mapped via xmap).
        store_selection_keys = "<Tab>",
      })

      require("luasnip.loaders.from_lua").lazy_load()

      -- <c-l> is selecting within a list of options.
      vim.keymap.set({ "s", "i" }, "<c-l>", function()
        if ls.choice_active() then
          ls.change_choice(1)
        end
      end, { desc = "Scroll through choice nodes" })

      vim.keymap.set("i", "<Tab>", function()
        return ls.expand_or_jumpable() and "<Plug>luasnip-expand-or-jump" or "<Tab>"
      end, { desc = "Expand or jump snippet", expr = true, silent = true })

      vim.keymap.set("i", "<S-Tab>", function()
        if ls.jumpable(-1) then
          ls.jump(-1)
        end
      end, { desc = "Jump backwards snippet" })
    end,
  },
  -- {
  --   "L3MON4D3/LuaSnip",
  --   event = "VeryLazy",
  --   config = function()
  --     require("luasnip.loaders.from_lua").load({ paths = "./snippets" })
  --   end,
  -- },

  -- Autocompletion
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-path" }, -- vim/neovim }snippet stuffs
      { "hrsh7th/cmp-cmdline" }, -- vim/n}eovim snippet stuffs
      { "saadparwaiz1/cmp_luasnip" },
      { "hrsh7th/cmp-nvim-lsp-signature-help" },
      { "windwp/nvim-autopairs" }, -- Auto }pairs
      { "PaterJason/cmp-conjure" },
    },
    config = function()
      -- Here is where you configure the autocompletion settings.
      local lsp_zero = require("lsp-zero")
      lsp_zero.extend_cmp()

      -- And you can configure cmp even more, if you want to.
      local cmp = require("cmp")
      -- local cmp_format = require("lsp-zero").cmp_format()
      local cmp_action = require("lsp-zero").cmp_action()
      local luasnip = require("luasnip")
      require("luasnip.loaders.from_vscode").lazy_load()

      cmp.setup({
        -- formatting = lsp_zero.cmp_format(),
        fields = { "abbr", "kind", "menu" },
        -- formatting = cmp_format,
        formatting = {
          fields = { "abbr", "kind", "menu" },
          format = require("lspkind").cmp_format({
            mode = "symbol", -- show only symbol annotations
            maxwidth = 50, -- prevent the popup from showing more than provided characters
            ellipsis_char = "...", -- when popup menu exceed maxwidth,
          }),
        },
        preselect = "none",
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        sources = {
          { name = "luasnip" },
          { name = "nvim_lsp" },
          { name = "nvim_lua" },
          { name = "path" },
          { name = "conjure" },
        },
        completion = {
          -- autocomplete = true
          completeopt = "menu,menuone,noinsert",
        },

        mapping = cmp.mapping.preset.insert({
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-u>"] = cmp.mapping.scroll_docs(-4),
          ["<C-d>"] = cmp.mapping.scroll_docs(4),
          ["<C-f>"] = cmp_action.luasnip_jump_forward(),
          ["<C-b>"] = cmp_action.luasnip_jump_backward(),
          ["<Down>"] = cmp.mapping(function(fallback)
            cmp.close()
            fallback()
          end, { "i" }),
          ["<Up>"] = cmp.mapping(function(fallback)
            cmp.close()
            fallback()
          end, { "i" }),
          ["<Tab>"] = cmp_action.tab_complete(),
          ["<S-Tab>"] = cmp_action.select_prev_or_fallback(),
          ["<CR>"] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = false,
          }),
          ["<ScrollWheelUp>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), {
            "i",
            "c",
          }),
          ["<ScrollWheelDown>"] = cmp.mapping(cmp.mapping.scroll_docs(4), {
            "i",
            "c",
          }),
        }),
      })
      cmp.setup.cmdline("/", {
        sources = {
          { name = "buffer" },
          { name = "treesitter" },
        },
      })

      cmp.setup.cmdline(":", {
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
      })
      cmp.setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
        sources = { name = "dap" },
      })
    end,
  },

  {
    "neovim/nvim-lspconfig",
    cmd = { "LspInfo", "LspInstall", "LspStart" },
    -- event = { "BufReadPre" },
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {

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
      { "simrat39/inlay-hints.nvim" },
      { "j-hui/fidget.nvim" },
      { "mfussenegger/nvim-lint" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "lukas-reineke/lsp-format.nvim" },
      { "onsails/lspkind.nvim" },
    },

    config = function()
      -- This is where all the LSP shenanigans will live

      local lsp_zero = require("lsp-zero")
      local ih = require("inlay-hints")
      ih.setup()

      lsp_zero.on_attach(function(_, bufnr)
        -- see :help lsp-zero-keybindings
        -- to learn the available actions
        lsp_zero.default_keymaps({ buffer = bufnr })
      end)
      lsp_zero.set_sign_icons({
        error = "✘",
        warn = "▲",
        hint = "⚑",
        info = "»",
      })

      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls", "ruff_lsp", "clangd", "neocmake" },
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
                ih.on_attach(client, bufnr)
              end,
              settings = {
                Lua = {
                  hint = {
                    enable = true,
                  },
                  diagnostics = {
                    globals = { "vim" },
                  },
                },
              },
            })
          end,

          -- Python Ruff Linter and Formatter
          ruff_lsp = function()
            require("lspconfig").ruff_lsp.setup({
              -- on_attach = on_attach,
              init_options = {
                settings = {
                  -- Any extra CLI arguments for `ruff` go here.
                  args = {},
                },
              },
            })
          end,

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
              on_attach = function()
                require("clangd_extensions.inlay_hints").setup_autocmd()
                require("clangd_extensions.inlay_hints").set_inlay_hints()
              end,
            })
          end,

          -- CMake File LSP
          neocmake = function()
            require("lspconfig").neocmake.setup({
              cmd = { "neocmakelsp", "--stdio" },
              filetypes = { "cmake" },
              root_dir = function()
                return lsp_zero.dir.find_first({ ".git", 'cmake' })
              end,
              single_file_support = true, -- suggested
            })
          end,
        },
      })
    end,
    -- lsp.setup()
  },

  -- DAPS
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      { "rcarriga/nvim-dap-ui" },
      { "theHamsta/nvim-dap-virtual-text" },
      { "jbyuki/one-small-step-for-vimkind" },
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")
      local osv = require("osv")
      vim.fn.sign_define("DapBreakpoint", { text = "🔴", texthl = "", linehl = "", numhl = "" })
      vim.fn.sign_define(
        "DapBreakpointCondition",
        { text = "🔵", texthl = "", linehl = "", numhl = "" }
      )
      require("dap.ext.vscode").load_launchjs()
      dapui.setup()
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.after.event_exited["dapui_config"] = function()
        dapui.close()
      end
      local osv_port = 8086
      if not dap.launch_server then
        dap.launch_server = {}
      end
      dap.configurations.lua = {
        {
          type = "nlua",
          request = "attach",
          name = "Attach to running Neovim instance",
        },
      }
      dap.configurations[""] = dap.configurations.lua
      dap.adapters.nlua = function(callback, config)
        callback({
          type = "server",
          host = config.host or "127.0.0.1",
          port = config.port or osv_port,
        })
      end
      dap.launch_server["nil"] = function()
        print("Starting OSV DAP Server")
        osv.launch({ port = osv_port })
      end
    end,
  },

  { "Olical/nfnl", ft = "fennel" },
  { "Olical/aniseed" },
  {
    "Olical/conjure",
    ft = { "clojure", "fennel" }, -- etc
    -- [Optional] cmp-conjure for cmp
    dependencies = {
      {
        "PaterJason/cmp-conjure",
        config = function()
          local cmp = require("cmp")
          local config = cmp.get_config()
          table.insert(config.sources, {
            name = "buffer",
            option = {
              sources = {
                { name = "conjure" },
              },
            },
          })
          cmp.setup(config)
        end,
      },
    },
    config = function(_, _)
      require("conjure.main").main()
      require("conjure.mapping")["on-filetype"]()
    end,
    init = function()
      -- Set configuration options here
      vim.g["conjure#debug"] = true
    end,
  },
  {
    "linux-cultist/venv-selector.nvim",
    dependencies = {
      "neovim/nvim-lspconfig",
      "nvim-telescope/telescope.nvim",
      "mfussenegger/nvim-dap-python",
    },
    opts = {
      -- Your options go here
      -- name = "venv",
      -- auto_refresh = false
    },
    event = "VeryLazy", -- Optional: needed only if you want to type `:VenvSelect` without a keymapping
    keys = {
      {
        -- Keymap to open VenvSelector to pick a venv.
        "<leader>vs",
        "<cmd>:VenvSelect<cr>",
        -- Keymap to retrieve the venv from a cache (the one previously used for the same project directory).
        "<leader>vc",
        "<cmd>:VenvSelectCached<cr>",
      },
    },
  },
  {
    "linux-cultist/venv-selector.nvim",
    dependencies = {
      "neovim/nvim-lspconfig",
      "nvim-telescope/telescope.nvim",
      "mfussenegger/nvim-dap-python",
    },
    -- dev = true,
    enabled = true,
    config = true,
    event = "VeryLazy",
    -- event = "VeryLazy", -- This is only needed if you want to run :VenvSelect without keymappings
    -- config = {
    -- 	function()
    -- 		require("venv-selector").setup({
    --
    -- 			notify_user_on_activate = true,
    -- 		})
    -- 	end
    keys = { {
      "<localleader>v",
      "<cmd>VenvSelect<cr>",
    } },
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
      require("cmake-tools").setup({
        cmake_build_directory = "build",
      })
    end,
  },
  {
    "mrcjkb/haskell-tools.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "akinsho/toggleterm.nvim",
    },
    ft = { "haskell", "cabal" },
    config = function()
      require("haskell-tools").setup({
        hls = {
          on_attach = function()
            require("pokerus.lsp").on_attach()
          end,
          settings = {
            haskell = {
              formattingProvider = "fourmolu",
              plugin = {
                stan = { globalOn = false },
              },
            },
          },
        },
        repl = { handler = "toggleterm" },
      })
    end,
  },
  -- {
  --   "mhartington/formatter.nvim",
  --   cmd = { "Format", "FormatWrite" },
  --   config = function()
  --     require("formatter").setup({
  --       filetype = {
  --         c = { require("formatter.filetypes.c").clangformat },
  --         cpp = { require("formatter.filetypes.c").clangformat },
  --         css = { require("formatter.filetypes.css").prettier },
  --         html = { require("formatter.filetypes.html").prettier },
  --         javascript = { require("formatter.filetypes.javascript").prettier },
  --         json = { require("formatter.filetypes.json").prettier },
  --         lua = { require("formatter.filetypes.lua").stylua },
  --         markdown = { require("formatter.filetypes.markdown").prettier },
  --         rust = { require("formatter.filetypes.rust").rustfmt },
  --         sh = { require("formatter.filetypes.sh").shfmt },
  --         toml = { require("formatter.filetypes.toml").taplo },
  --       },
  --     })
  --   end,
  -- },

  -- Linting and Formatting
  {
    "nvimdev/guard.nvim",
    -- enabled = false,
    config = function()
      local ft = require("guard.filetype")

      ft("c"):fmt("clang-format"):lint("clang-tidy")

      ft("lua"):fmt("stylua")

      -- Call setup() LAST!
      require("guard").setup({
        -- the only options for the setup function
        fmt_on_save = true,
        -- Use lsp if no formatter was defined for this filetype
        lsp_as_default_formatter = false,
      })
    end,
  },
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
  { "simrat39/rust-tools.nvim" },
  { "NoahTheDuke/vim-just" },
  { "rafcamlet/nvim-luapad" },
  { -- This plugin
    "Zeioth/compiler.nvim",
    cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
    dependencies = { "stevearc/overseer.nvim" },
    opts = {},
    keys = {
      { "<F6>", "<cmd>CompilerOpen<CR>" },
    },
  },
  { -- The task runner we use
    "stevearc/overseer.nvim",
    commit = "19aac0426710c8fc0510e54b7a6466a03a1a7377",
    cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
    opts = {
      task_list = {
        direction = "bottom",
        min_height = 25,
        max_height = 25,
        default_detail = 1,
        bindings = {
          ["q"] = function()
            vim.cmd("OverseerClose")
          end,
        },
      },
    },
  },
}
