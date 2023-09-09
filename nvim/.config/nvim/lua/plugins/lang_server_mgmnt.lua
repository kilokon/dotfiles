return {
  {
    "VonHeikemen/lsp-zero.nvim",
    branch = "dev-v3",
    lazy = true,
    config = false,
    init = function()
      -- Disable automatic setup, we are doing it manually
      vim.g.lsp_zero_extend_cmp = 0
      vim.g.lsp_zero_extend_lspconfig = 0
      vim.b.lsp_zero_enable_autoformat = 0
    end,
  },

  {
    "L3MON4D3/LuaSnip",
    event = "VeryLazy",
    config = function()
      require("luasnip.loaders.from_lua").load({ paths = "./snippets" })
    end,
  },

  -- Autocompletion
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      { "hrsh7th/cmp-nvim-lsp" },
      { "L3MON4D3/LuaSnip" },
      { "hrsh7th/cmp-path" }, -- vim/neovim }snippet stuffs
      { "hrsh7th/cmp-cmdline" }, -- vim/n}eovim snippet stuffs
      { "hrsh7th/cmp-nvim-lsp-signature-help" },
      { "windwp/nvim-autopairs" }, -- Auto }pairs
      { "PaterJason/cmp-conjure" },
    },
    config = function()
      -- Here is where you configure the autocompletion settings.
      local lsp_zero = require("lsp-zero")
      lsp_zero.extend_cmp()
      lsp_zero.format_on_save({
        format_opts = {
          async = false,
          timeout_ms = 10000,
        },
        servers = {
          ["ruff_lsp"] = { "python" },
          ["stylua"] = { "lua" },
          ["tsserver"] = { "javascript", "typescript" },
          ["rust_analyzer"] = { "rust" },
        },
      })

      -- And you can configure cmp even more, if you want to.
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local cmp_action = lsp_zero.cmp_action()

      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0
          and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s")
            == nil
      end

      cmp.setup({
        formatting = lsp_zero.cmp_format(),
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
        -- completion = {
        -- 	autocomplete = true
        -- },
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
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            elseif has_words_before() then
              cmp.complete()
              -- elseif luasnip.jumpable(1) then
              -- 	luasnip.jump(1)
            else
              fallback()
            end
          end),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end),
          ["<cr>"] = cmp.mapping.confirm({
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
        -- sorting = {
        -- 	comparators = {
        -- 		cmp.config.compare.offset,
        -- 		cmp.config.compare.exact,
        -- 		cmp.config.compare.recently_used,
        -- 		-- require("clangd_extensions.cmp_scores"),
        -- 		cmp.config.compare.kind,
        -- 		cmp.config.compare.sort_text,
        -- 		cmp.config.compare.length,
        -- 		cmp.config.compare.order,
        -- 	},
        -- },
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
      { "simrat39/rust-tools.nvim" },
      {
        "Civitasv/cmake-tools.nvim",
        dependencies = {
          "stevearc/overseer.nvim",
        },
        config = function()
          require("cmake-tools").setup({
            cmake_build_directory = "build",
          })
        end,
      },
      -- DAPS
      { "rcarriga/nvim-dap-ui" }, -- UI fo}r Dap
      { "mfussenegger/nvim-dap" }, -- Debug}ger, setup below
      { "mfussenegger/nvim-lint" }, -- Neovi}m linter
      { "mhartington/formatter.nvim" }, -- Neovi}m formatter
      { "hrsh7th/cmp-nvim-lsp" }, -- Neovim }LSP feeder for cmp
      { "jbyuki/one-small-step-for-vimkind" }, -- Neovim }Dap
    },

    config = function()
      -- This is where all the LSP shenanigans will live
      require("formatter").setup({
        filetype = {
          ["*"] = {
            require("formatter.filetypes.any"),
          },
          lua = {
            -- You can also define your own configuration
            function()
              local util = require("formatter.util")
              -- Full specification of configurations is down below and in Vim help
              -- files
              return {
                exe = "stylua",
                args = {
                  "--indent-type",
                  "Spaces",
                  "--search-parent-directories",
                  "--stdin-filepath",
                  util.escape_path(util.get_current_buffer_file_path()),
                  "--",
                  "-",
                },
                stdin = true,
              }
            end,
          },
        },
      })

      local lsp_zero = require("lsp-zero")
      local ih = require("inlay-hints")
      local dap = require("dap")
      local dapui = require("dapui")
      local osv = require("osv")
      lsp_zero.extend_lspconfig()

      lsp_zero.on_attach(function(client, bufnr)
        -- see :help lsp-zero-keybindings
        -- to learn the available actions
        lsp_zero.default_keymaps({
          buffer = bufnr,
          omit = { "K" },
        })
        if client.server_capabilities.documentSymbolProvider then
          require("nvim-navic").attach(client, bufnr)
        end
      end)

      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls", "ruff_lsp", "clangd" },
        handlers = {
          -- lsp_zero.default_setup,
          lua_ls = function()
            -- (Optional) Configure lua language server for neovim
            local lua_opts = lsp_zero.nvim_lua_ls()
            require("lspconfig").lua_ls.setup({
              lua_opts,
              root_dir = function()
                --- project root will be the first directory that has
                --- either .luarc.json or .stylua.toml
                return lsp_zero.dir.find_first({ ".luarc.json", ".stylua.toml" })
              end,
              on_attach = function(client, bufnr)
                -- on_attach(client, bufnr)
                print("hello from lua_ls")
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
          clangd = function()
            require("lspconfig").clangd.setup({
              -- on_attach = on_attach
            })
          end,
        },
      })
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
}

-- vim.api.nvim_create_autocmd("VimEnter", {
-- 	desc = "Auto select virtualenv Nvim open",
-- 	pattern = "*",
-- 	callback = function()
-- 		local venv = vim.fn.findfile("pyproject.toml", vim.fn.getcwd() .. ";")
-- 		if venv ~= "" then
-- 			require("venv-selector").retrieve_from_cache()
-- 		end
-- 	end,
-- 	once = true,
-- })
