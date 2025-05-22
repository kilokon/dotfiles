return {
  "epwalsh/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  name = "obsidian",
  lazy = true,
  ft = "markdown",
  keys = {
    { "K", false },
  },
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  -- event = {
  -- --   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
  -- --   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/*.md"
  -- --   -- refer to `:h file-pattern` for more examples
  --   "BufReadPre path/to/my-vault/*.md",
  --   "BufNewFile path/to/my-vault/*.md",
  -- },
  dependencies = {
    -- Required.
    "nvim-lua/plenary.nvim",
    "ibhagwan/fzf-lua",
    "hrsh7th/nvim-cmp",
    "nvim-telescope/telescope.nvim",
  },
  -- config = function(_, opts)
  --   require("obsidian").setup({ opts })
  --   local wk = require("which-key")
  --
  --   wk.add({
  --     { "<leader>o", group = "Obsidian" },
  --     { "<leader>oo", "<cmd>ObsidianOpen<cr>", desc = "Open note" },
  --   })
  -- end,
  opts = {
    workspaces = {
      {
        name = "Geme Dev",
        path = "~/OneDrive/doc/Obsidian/Game Dev",
        overrides = {
          notes_subdir = "notes",
        },
      },
      {
        name = "Programming",
        path = "/home/aviik/OneDrive/doc/Obsidian/Programming",
      },
    },
  },
}
