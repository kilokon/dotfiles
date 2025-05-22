local config = require("lazy.core.config")
local cfg = {
  configurations = {
    -- C lang configurations
    c = {
      {
        name = "Launch debugger",
        type = "lldb",
        request = "launch",
        cwd = "${workspaceFolder}",
        program = function()
          -- Build with debug symbols
          local out = vim.fn.system({ "make", "debug" })
          -- Check for errors
          if vim.v.shell_error ~= 0 then
            vim.notify(out, vim.log.levels.ERROR)
            return nil
          end
          -- Return path to the debuggable program
          return "lldb"
        end,
      },
    },
  },
}
return {
  {
    "mfussenegger/nvim-dap",
    lazy = true,
    dependencies = {
      { "theHamsta/nvim-dap-virtual-text", config = true },
      -- { "mortepau/codicons.nvim" },
      { "nvim-telescope/telescope-dap.nvim" },
      -- { "stevearc/overseer.nvim" },
      -- { "julianolf/nvim-dap-lldb" },
      {
        "Weissle/persistent-breakpoints.nvim",
        -- event = { "BufReadPost" },
        keys = {
          { "<leader>da", "<cmd>lua require('persistent-breakpoints.api').toggle_breakpoint()<cr>" },
          { "<leader>dC", "<cmd>lua require('persistent-breakpoints.api').clear_all_breakpoints()<cr>" },
        },
        -- opts = { load_breakpoints_event = { "BufReadPost" } },
        config = function()
          require("persistent-breakpoints").setup({
            load_breakpoints_event = { "BufReadPost" },
          })
        end,
      },
      {
        "rcarriga/nvim-dap-ui",
        dependencies = { "nvim-neotest/nvim-nio", "folke/lazydev.nvim", "mfussenegger/nvim-dap" },
        keys = {
          { "<leader>dU", "<cmd>lua require'dapui'.toggle()<cr>", desc = "Toggle UI" },
          { "<leader>dh", "<cmd>lua require'dap.ui.widgets'.hover()<cr>", desc = "Hover Variables" },
          { "<leader>de", "<cmd>lua require'dapui'.eval()<cr>", desc = "Evaluate" },
        },
        config = function()
          local dap, dapui = require("dap"), require("dapui")

          dapui.setup({
            controls = {
              element = "repl",
              enabled = true,
              icons = {
                disconnect = "",
                pause = "",
                play = "",
                run_last = "",
                step_back = "",
                step_into = "",
                step_out = "",
                step_over = "",
                terminate = "",
              },
            },
            element_mappings = {},
            expand_lines = true,
            floating = {
              border = "rounded",
              mappings = {
                close = { "q", "<Esc>" },
              },
            },
            force_buffers = true,
            icons = {
              collapsed = "",
              current_frame = "",
              expanded = "",
            },
            layouts = {
              {
                elements = {
                  {
                    id = "scopes",
                    size = 0.25,
                  },
                  {
                    id = "breakpoints",
                    size = 0.25,
                  },
                  {
                    id = "stacks",
                    size = 0.25,
                  },
                  {
                    id = "watches",
                    size = 0.25,
                  },
                },
                position = "right",
                size = 50,
              },
              {
                elements = {
                  {
                    id = "repl",
                    size = 0.5,
                  },
                  {
                    id = "console",
                    size = 0.5,
                  },
                },
                position = "bottom",
                size = 10,
              },
            },
            mappings = {
              edit = "e",
              expand = { "<CR>", "<2-LeftMouse>" },
              open = "o",
              remove = "d",
              repl = "r",
              toggle = "t",
            },
            render = {
              indent = 1,
              max_value_lines = 100,
            },
          })

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
          vim.fn.sign_define("DapBreakpoint", { text = "○", texthl = "DiagnosticError", linehl = "", numhl = "" })
          vim.fn.sign_define(
            "DapBreakpointCondition",
            { text = "?", texthl = "DiagnosticError", linehl = "", numhl = "" }
          )
          vim.fn.sign_define("DapLogPoint", { text = "⁋", texthl = "", linehl = "", numhl = "" })
          vim.fn.sign_define("DapStopped", { text = " ", texthl = "DiagnosticInfo", linehl = "", numhl = "" })
          vim.fn.sign_define(
            "DapBreakpointRejected",
            { text = "X", texthl = "DiagnosticError", linehl = "", numhl = "" }
          )
          ---@diagnostic disable-next-line: missing-fields
          dapui.setup({})
        end,
      },
      -- {
      --   "igorlfs/nvim-dap-view",
      --   opts = {},
      -- },
    },
    keys = {
      -- {
      --   "<F5>",
      --   "<cmd>lua local continue = function() if vim.fn.filereadable('.vscode/launch.json') then require('dap.ext.vscode').load_launchjs(nil, { coreclr = { 'cs' } }) end require('dap').continue() end continue()<cr>",
      --   desc = "Continue (F5)",
      -- },
      { "<leader>db", "<cmd>lua require'dap'.step_back()<cr>", desc = "Step Back" },
      -- { "<leader>dd", "<cmd>lua require'dap'.disconnect()<cr>", desc = "Disconnect" },
      -- { "<leader>dg", "<cmd>lua require'dap'.session()<cr>", desc = "Get Session" },
      { "<leader>di", "<cmd>lua require'dap'.step_into()<cr>", desc = "Step Into" },
      { "<leader>do", "<cmd>lua require'dap'.step_over()<cr>", desc = "Step Over" },
      { "<leader>dp", "<cmd>lua require'dap'.pause()<cr>", desc = "Pause" },
      { "<leader>dq", "<cmd>lua require'dap'.close()<cr>", desc = "Quit" },
      { "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>", desc = "Toggle Repl" },
      { "<leader>ds", "<cmd>lua require'dap'.continue()<cr>", desc = "Start" },
      -- { "<leader>dy", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", desc = "Toggle Breakpoint" },
      -- { "<leader>dx", "<cmd>lua require'dap'.terminate()<cr>", desc = "Terminate" },
      { "<leader>du", "<cmd>lua require'dap'.step_out()<cr>", desc = "Step Out" },
    },

    config = function()
      local dap = require("dap")
      dap.defaults.fallback.exception_breakpoints = { "uncaught" }
      dap.set_log_level("DEBUG")

      -- codelldb C, C++, Rust
      dap.adapters.codelldb = {
        type = "executable",
        command = "codelldb",
      }
      -- lldb-dap C, C++ , Rust
      dap.adapters.lldb = {
        type = "executable",
        command = "/usr/bin/lldb-dap",
        name = "lldb",
      }
      -- bash
      dap.adapters.bashdb = {
        type = "executable",
        command = vim.fn.stdpath("data") .. "/mason/packages/bash-debug-adapter/bash-debug-adapter",
        name = "bashdb",
      }
      -- cpplldb (working with C)
      dap.adapters.cppdbg = {
        id = "cppdbg",
        type = "executable",
        command = "/home/aviik/.local/share/nvim/dap-vscode-cpptools/extension/debugAdapters/bin/OpenDebugAD7",
      }
      -- python
      dap.adapters.python = {
        type = "executable",
        command = "python",
        args = { "-m", "debugpy.adapter" },
      }

      dap.configurations.c = {
        {
          name = "Launch file",
          type = "cppdbg",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = true,
        },
        {
          name = "Attach to gdbserver: 1234",
          type = "cppdbg",
          request = "launch",
          MIMode = "gdb",
          miDebuggerSeverAddress = "localhost:1234",
          miDebuggerPath = "/usr/bin/gdb",
          cwd = "${workspaceFolder}",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
        },
        {
          name = "LLDB-DAP",
          type = "lldb",
          request = "launch",
          cwd = "${workspaceFolder}",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          stopOnEntry = false,
        },
      }

      dap.configurations.cpp = {
        {
          name = "Launch",
          type = "codelldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
          args = {},
        },
      }

      for key, _ in pairs(dap.configurations) do
        dap.listeners.on_config[key] = convertArgStringToArray
      end

      local continue = function()
        -- support for vscode launch.json is partial.
        -- not all configuration options and features supported
        if vim.fn.filereadable(".vscode/launch.json") then
          require("dap.ext.vscode").load_launchjs()
        end
        dap.continue()
      end

      vim.keymap.set("n", "<F5>", continue, { desc = "Continue" })
    end,
  },

  -- {
  --   "ChristianChiarulli/neovim-codicons",
  --   commit = "bd52f75",
  --   config = function()
  --     require("codicons").setup({})
  --   end,
  -- },
  -- persistent breakpionts
}
