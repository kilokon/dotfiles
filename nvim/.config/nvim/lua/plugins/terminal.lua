-- Filter the terminal by the provided user info.

return {
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    -- config = function()
    -- 	print("hello")
    -- end,
    opts = {
      shell = "/usr/bin/zsh",
      open_mapping = [[<c-\>]],
      size = 25,
    },
  },
  {
    -- TODO: Integrate with Hydra
    "willothy/wezterm.nvim",
    config = true,
    keys = {
      { "<leader>w", "<cmd>WeztermSpawn<CR>" },
    },
    opts = function()
      -- vim.keymap.set("n", "<leader>w", require("wezterm").switch_tab.relative)
    end,
  },
  {
    "willothy/flatten.nvim",
    config = true,
    -- or pass configuration with
    -- opts = {  }
    -- Ensure that it runs first to minimize delay when opening file from terminal
    lazy = false,
    priority = 1001,
  },
}
