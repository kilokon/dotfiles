return {
  { "mfussenegger/nvim-dap-python" },
  {
    "neolooong/whichpy.nvim",
    ft = { "python" },
    dependencies = {
      -- optional for dap
      -- "mfussenegger/nvim-dap-python",
      -- optional for picker support
      -- "ibhagwan/fzf-lua",
      -- "nvim-telescope/telescope.nvim",
    },
    opts = {},
  },
}
