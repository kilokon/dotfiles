return {
  "rouge8/neotest-rust",
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-rust")({
          args = { "--no-capture" },
          dap_adapter = "lldb",
        }),
      },
    })
  end,
}
