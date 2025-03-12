return {
  "tzachar/local-highlight.nvim",
  config = function()
    require("local-highlight").setup({
      animate = {
        enabled = false,
        easing = "linear",
        duration = {
          step = 10, -- ms per step
          total = 100, -- maximum duration
        },
      },
    })
  end,
}
