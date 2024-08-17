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
 {
  'kaarmu/typst.vim',
  ft = 'typst',
  lazy=false,
},
--   {
--   'chomosuke/typst-preview.nvim',
--   lazy = false, -- or ft = 'typst'
--   version = '0.3.*',
--   build = function() require 'typst-preview'.update() end,
-- }
  -- {
  --   "stevearc/gkeep.nvim",
  --   build = "UpdateRemotePlugins",
  --   opts = {},
  --   -- Optional dependencies
  --   dependencies = { "nvim-tree/nvim-web-devicons" },
  -- },
}
