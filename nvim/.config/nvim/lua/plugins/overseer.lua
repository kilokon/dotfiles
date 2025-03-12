return {
  {
    "stevearc/overseer.nvim",
    lazy = true,
    event = { "VimEnter" },
    config = function()
      require("overseer").setup({
        templates = {
          "builtin",
          "kilokon.cpp_build",
          -- 'my.tsc',
        },
        strategy = "toggleterm",
      })
    end,
  },
}
