return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  opts = {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  },
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
  config = function()
    local wk = require("which-key")
    wk.add({
      { "<leader>f", group = "file" }, -- group
      { "<leader>e", group = "explore" },
      { "<leader>g", group = "lsp" },
      { "<leader>a", group = "Edit" },
      { "<leader>as", group = "Snippets", nowait = false, remap = false },
      { "<leader>ar", group = "Refactor" },
    })

    wk.add({
      -- NeoTree
      { "<leader>ee", "<cmd>Neotree reveal<cr>", desc = "NeoTree" },
      {
        "<leader>eb",
        function()
          require("neo-tree.command").execute({
            toggle = true,
            source = "buffers",
            position = "left",
          })
        end,
        desc = "Buffers (root dir)",
      },
      {
        "<leader>ef",
        function()
          require("neo-tree.command").execute({
            toggle = true,
            source = "filesystem",
            position = "left",
          })
        end,
        desc = "filesystem (root dir)",
      },

      {
        "<leader>eg",
        function()
          require("neo-tree.command").execute({
            toggle = true,
            source = "git_status",
            position = "left",
          })
        end,
        desc = "Git Status (root dir)",
      },
      -- Scissors (Snippets)
      {
        mode = { "n", "x" },
        {
          "<leader>ase",
          '<cmd>lua require("scissors").editSnippet()<CR>',
          desc = "Edit snippet",
          nowait = false,
          remap = false,
        },
      },
      {
        "<leader>asa",
        '<cmd>lua require("scissors").addNewSnippet()<CR>',
        desc = "Add new snippet",
        nowait = false,
        remap = false,
      },

      -- Refactoring
      {
        mode = "x",
        {
          "<leader>are",
          function()
            require("refactoring").refactor("Extract Function")
          end,
          desc = "Refactor Extract",
        },
        {
          "<leader>arf",
          function()
            require("refactoring").refactor("Extract Function To File")
          end,
          desc = "Refactor Extract to File",
        },
        {
          "<leader>arv",
          function()
            require("refactoring").refactor("Extract Variable")
          end,
          desc = "Extract Variable",
        },
        {
          "<leader>ari",
          function()
            require("refactoring").refactor("Inline Variable")
          end,
          desc = "Refactor Inline Variable",
        },
      },
      {
        "<leader>arI",
        function()
          require("refactoring").refactor("Inline Function")
        end,
        desc = "Refactor Block",
      },
      {
        "<leader>arb",
        function()
          require("refactoring").refactor("Extract Block")
        end,
        desc = "Refactor Block",
      },
      {
        "<leader>arF",
        function()
          require("refactoring").refactor("Extract Block To File")
        end,
        desc = "Extract Block to File",
      },
      -- Glance
      -- {
      --   mode = "n",
      --   {
      --     "gD",
      --     "<CMD>Glance definitions<CR>",
      --     desc = "Glance Definitions",
      --   },
      --   {
      --     "gR",
      --     "<CMD>Glance references<CR>",
      --     desc = "Glance References",
      --   },
      --   {
      --     "gY",
      --     "<CMD>Glance type_definitions<CR>",
      --     desc = "Glance Type Definitions",
      --   },
      --   {
      --     "gM",
      --     "<CMD>Glance implementations<CR>",
      --     desc = "Glance Implementations",
      --   },
      -- },
      -- Smart Splits
      -- { "<>" },
    })
  end,
}
