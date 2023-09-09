return {
  {
    "frabjous/knap",
    keys = {
      {
        "<leader>vk",
        function()
          require("knap").toggle_autopreviewing()
          vim.keymap.set("n", "<localleader>v", require("knap").forward_jump, { desc = "Fwd Jump" })
        end,
        desc = "Knap",
      },
    },
  },
  -- {
  --   "stevearc/gkeep.nvim",
  --   build = "UpdateRemotePlugins",
  --   opts = {},
  --   -- Optional dependencies
  --   dependencies = { "nvim-tree/nvim-web-devicons" },
  -- },
}
