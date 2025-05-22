return {
  "jinzhongjia/LspUI.nvim",
  event = "LspAttach",
  branch = "main",
  config = function()
    require("LspUI").setup({
      -- config options go here
    })
  end,
}
