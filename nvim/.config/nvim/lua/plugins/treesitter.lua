return {
  {
    'RRethy/nvim-treesitter-endwise',
    event = "InsertEnter",
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
    }
  },
  {
    "nvim-treesitter/nvim-treesitter",
    -- event = { "BufRead", "BufNewFile", "InsertEnter" },
    event = "VeryLazy",
    -- event = "User BaseFile",
    -- event = { "BufReadPost", "BufNewFile" },
    -- cmd = {
    -- 	"TSBufDisable",
    -- 	"TSBufEnable",
    -- 	"TSBufToggle",
    -- 	"TSDisable",
    -- 	"TSEnable",
    -- 	"TSToggle",
    -- 	"TSInstall",
    -- 	"TSInstallInfo",
    -- 	"TSInstallSync",
    -- 	"TSModuleInfo",
    -- 	"TSUninstall",
    -- 	"TSUpdate",
    -- 	"TSUpdateSync",
    -- },
    dependencies = {
      { "nvim-treesitter/playground" },
      'p00f/nvim-ts-rainbow',
      'm-demare/hlargs.nvim',
      -- { "nvim-treesitter/nvim-treesitter-textobjects", event = "CursorHold" },
      { "hiphish/rainbow-delimiters.nvim",     event = "VeryLazy" },
      -- { "/nvim-ts-rainbow2", event = "VeryLazy" },

      { "mimmanuel/nvim-treesitter-powershell" },
      { "nushell/tree-sitter-nu" },
    },
    build = ":TSUpdate",
    config = function()
      local configs = require("nvim-treesitter.configs")
      local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
      local parsers = require("nvim-treesitter.parsers")
      local rainbow_enabled_list = { "json" }

      require("nvim-treesitter.parsers").get_parser_configs().just = {
        install_info = {
          url = "https://github.com/IndianBoy42/tree-sitter-just", -- local path or git repo
          files = { "src/parser.c", "src/scanner.cc" },
          branch = "main",
          -- use_makefile = true -- this may be necessary on MacOS (try if you see compiler errors)
        },
        filetype = "justfile",
        maintainers = { "@IndianBoy42" },
      }
      require("nvim-treesitter.parsers").get_parser_configs().c3 = {
        install_info = {
          url = "https://github.com/c3lang/tree-sitter-c3", -- local path or git repo
          files = { "src/parser.c", "src/scanner.c" },
          branch = "main",
          use_makefile = true -- this may be necessary on MacOS (try if you see compiler errors)
        },
        filetype = "c3",
      }

      -- IMPORTANT: Install tree-sitter-cli
      parser_config.PowerShell = require("ts-powershell").parser_config

      configs.setup({
        ensure_installed = {
          "bash",
          "c",
          "comment",
          "csv",
          "cmake",
          "cuda",
          "dockerfile",
          "fennel",
          "fish",
          "glsl",
          "gitignore",
          "haskell",
          "hjson",
          "hlsl",
          "html",
          "javascript",
          "lua",
          "make",
          "ninja",
          "markdown",
          "meson",
          "nix",
          "python",
          "ron",
          "rust",
          "toml",
          "vim",
          "vimdoc",
          "yaml",
          "wgsl_bevy"
        },
        query_linter = {
          enable = true,
          use_virtual_text = true,
          lint_events = { "BufWrite", "CursorHold" },
        },
        sync_install = false,
        highlight = { enable = true },
        indent = { enable = true },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<C-space>",
            node_incremental = "<C-space>",
            scope_incremental = false,
            node_decremental = "<bs>",
          },
        },
        matchup = {
          enable = true, -- mandatory, false will disable the whole extension
          -- disable = { "yaml" },
        },
        rainbow = {
          enable = true,
          disable = vim.tbl_filter(
            function(p)
              local disable = true
              for _, lang in pairs(rainbow_enabled_list) do
                if p == lang then disable = false end
              end
              return disable
            end,
            parsers.available_parsers()
          ),
          extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
          max_file_lines = nil, -- Do not enable for files with more than n lines, int
          -- colors = {}, -- table of hex strings
          -- termcolors = {} -- table of colour name strings
        },
      })
    end,
  },
  {
    'm-demare/hlargs.nvim',
    lazy = true,
    config = function()
      require('hlargs').setup({
        hl_priority = 1000,
      })
    end
  },
  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    event = "CursorHold",
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
    },
  },
  {
    "chrisgrieser/nvim-various-textobjs",
    keys = {
      { "<leader><leader>", "<cmd>lua require('various-textobjs').url()<CR>" }
    },
    lazy = false,
    -- opts = { useDefaultKeymaps = true },
  },
  -- { "chrisgrieser/nvim-spider", lazy = true },
  -- keymaps in legendary nvim
}
