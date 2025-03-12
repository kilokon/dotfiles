return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
    "MunifTanjim/nui.nvim",
    -- { "3rd/image.nvim", opts = {} }, -- Optional image support in preview window: See `# Preview Mode` for more information
    "saifulapm/neotree-file-nesting-config",
  },
  config = function(_, opts)
    opts.nesting_rules = require("neotree-file-nesting-config").nesting_rules
    -- require("neo-tree").setup(opts)
    require("neo-tree").setup({
      window = {
        mappings = {
          --["<cr>"] = "open_split",
          ["<cr>"] = "open_vsplit",
        },
      },
      filesystem = {
        filtered_items = {
          show_hidden_count = false,
          never_show = {
            ".DS_Store",
          },
        },
        window = {
          mappings = {
            ["o"] = "system_open",
          },
        },
      },
      commands = {
        system_open = function(state)
          local node = state.tree:get_node()

          local path = node:get_id()
          if node and node.type == "file" then
            local cmds = require("neo-tree.sources.filesystem.commands")
            cmds.open(state)
            cmds.clear_filter(state)
            -- reveal the selected file without focusing the tree
            require("neo-tree.sources.filesystem").navigate(state, state.path, path)
          else
            vim.fn.jobstart({ "xdg-open", path }, { detach = true })
          end
        end,
      },
      event_handlers = {
        {
          event = "neo_tree_buffer_enter",
          handler = function()
            vim.cmd("highlight! Cursor blend=100")
          end,
        },
        {
          event = "neo_tree_buffer_leave",
          handler = function()
            vim.cmd("highlight! Cursor guibg=#5f87af blend=0")
          end,
        },

        {
          event = "file_open_requested",
          handler = function()
            -- auto close
            -- vim.cmd("Neotree close")
            -- OR
            require("neo-tree.command").execute({ action = "close" })
          end,
        },
      },
      enable_git_status = true,
      enable_diagnostics = true,
      hide_root_node = true,
      retain_hidden_root_indent = true,

      default_component_configs = {
        indent = {
          with_expanders = true,
          expander_collapsed = "",
          expander_expanded = "",
        },
      },
    })
  end,
  -- opts = {
  --   -- recommanded config for better UI
  --
  --   -- others config
  -- },
}
