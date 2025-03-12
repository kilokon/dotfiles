return {
  {
    "rcarriga/nvim-dap-ui",
    dependencies = { "mfussenegger/nvim-dap", "nvim-neotest/nvim-nio", "folke/lazydev.nvim" },
    config = function()
      local dap, dapui = require("dap"), require("dapui")
      dap.listeners.before.attach.dapui_config = function()
        dapui.open()
      end
      dap.listeners.before.launch.dapui_config = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated.dapui_config = function()
        dapui.close()
      end
      dap.listeners.before.event_exited.dapui_config = function()
        dapui.close()
      end

      dapui.setup({
        controls = { enabled = false },
        mappings = {
          edit = "E",
          expand = "<CR>",
          open = "o",
          remove = { "d", "x" },
          repl = "r",
          toggle = "t",
        },
        element_mappings = {
          breakpoints = { open = { "<CR>", "o" } },
          stacks = { open = { "o", "<CR>" } },
        },
        layouts = {
          {
            elements = {
              { id = "scopes", size = 0.25 },
              { id = "breakpoints", size = 0.25 },
              { id = "stacks", size = 0.25 },
              { id = "watches", size = 0.25 },
            },
            position = "right",
            size = 0.2,
          },
          {
            elements = {
              { id = "repl", size = 0.3 },
              { id = "console", size = 0.7 },
            },
            position = "right",
            size = 0.2,
          },
        },
        icons = {},
        expand_lines = true, -- default
        force_buffers = true, -- default
        floating = { border = "single", mappings = { close = { "q", "<Esc>" } } },
        render = { indent = 1, max_value_lines = 100 },
      })
    end,
    -- config = require("lazydev").setup({
    --   library = { "nvim-dap-ui" },
    -- }),
  },
  -- virtual text
  {
    "theHamsta/nvim-dap-virtual-text",
    opts = { commented = true, virt_text_pos = "eol" },
    -- config = true,
  },

  -- persistent breakpionts
  {
    "Weissle/persistent-breakpoints.nvim",
    opts = { load_breakpoints_event = { "BufReadPost" } },
  },
}
