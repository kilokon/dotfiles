return {
  {
    "cordx56/rustowl",
    dependencies = { "neovim/nvim-lspconfig" },
    keys = {
      { "<c-o", "<cmd>require('rustowl').rustowl_cursor<CR>" },
    },
    config = function()
      -- keymap("n", "<c-o>", require("rustowl").rustowl_cursor, { noremap = true, silent = true })
      local lspconfig = require("lspconfig")
      lspconfig.rustowl.setup({
        trigger = {
          hover = false,
        },
      })
    end,
  },
  {
    "vxpm/ferris.nvim",
    opts = {
      -- your options here
      -- If true, will automatically create commands for each LSP method
      create_commands = true, -- bool
      -- Handler for URL's (used for opening documentation)
      url_handler = "xdg-open", -- string | function(string)
    },
  },
  {
    "mrcjkb/rustaceanvim",
    version = "^5", -- Recommended
    lazy = false, -- This plugin is already lazy
    keys = {
      { "<leader>rr", false },
      { "<leader>rd", false },
    },
  },
  {
    "saecki/crates.nvim",
    tag = "stable",
    config = function()
      require("crates").setup({})
    end,
  },
}
