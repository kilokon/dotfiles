return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "User BaseFile",
    cmd = {
      "TSBufDisable",
      "TSBufEnable",
      "TSBufToggle",
      "TSDisable",
      "TSEnable",
      "TSToggle",
      "TSInstall",
      "TSInstallInfo",
      "TSInstallSync",
      "TSModuleInfo",
      "TSUninstall",
      "TSUpdate",
      "TSUpdateSync",
    },
    dependencies = {
      { "nvim-treesitter/playground" },

      { "mimmanuel/nvim-treesitter-powershell" },
    },
    build = ":TSUpdate",
    config = function()
      local configs = require("nvim-treesitter.configs")
      local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

      require("nvim-treesitter.parsers").get_parser_configs().just = {
        install_info = {
          url = "https://github.com/IndianBoy42/tree-sitter-just", -- local path or git repo
          files = { "src/parser.c", "src/scanner.cc" },
          branch = "main",
          -- use_makefile = true -- this may be necessary on MacOS (try if you see compiler errors)
        },
        maintainers = { "@IndianBoy42" },
      }

      -- IMPORTANT: Install tree-sitter-cli
      parser_config.PowerShell = require("ts-powershell").parser_config

      configs.setup({
        ensure_installed = {
          "bash",
          "c",
          "cpp",
          "csv",
          "cmake",
          "dockerfile",
          "fennel",
          "fish",
          "gitignore",
          "haskell",
          "html",
          "just",
          "lua",
          "make",
          "meson",
          "nix",
          "PowerShell",
          "python",
          "qmljs",
          "query",
          "ron",
          "rust",
          "toml",
          "vim",
          "vimdoc",
          "javascript",
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
      })
    end,
  },
  {
    "chrisgrieser/nvim-various-textobjs",
    lazy = false,
    opts = { useDefaultKeymaps = true },
  },
  { "chrisgrieser/nvim-spider", lazy = true },
  -- keymaps in legendary nvim
}
