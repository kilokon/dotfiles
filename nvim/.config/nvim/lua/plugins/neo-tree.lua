local function getTelescopeOpts(state, path)
  return {
    cwd = path,
    search_dirs = { path },
    attach_mappings = function(prompt_bufnr, map)
      local actions = require("telescope.actions")
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local action_state = require("telescope.actions.state")
        local selection = action_state.get_selected_entry()
        local filename = selection.filename
        if filename == nil then
          filename = selection[1]
        end
        -- any way to open the file without triggering auto-close event of neo-tree?
        require("neo-tree.sources.filesystem").navigate(state, state.path, filename)
      end)
      return true
    end,
  }
end
--
--
--
--
--
--
require("neo-tree").setup({
  source_selector = {
    winbar = false,
    statusline = false,
  },
  default_component_configs = {
    indent = {
      with_expanders = true,
      expander_collapsed = "",
      expander_expanded = "",
    },
  },
  event_handlers = {
    --
    --
    -- Hide mose cursor in Neo Tree
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
    --
    --
    {
      event = "file_opened",
      handler = function(file_path)
        --auto close
        require("neo-tree.command").execute({ action = "close" })
      end,
    },
    {
      event = "file_opened",
      handler = function(file_path)
        --clear search after opening a file
        require("neo-tree.sources.filesystem").reset_search(state)
      end,
    },
    {
      event = "file_renamed",
      handler = function(args)
        -- fix references to file
        print(args.source, " renamed to ", args.destination)
      end,
    },
    {
      event = "file_moved",
      handler = function(args)
        -- fix references to file
        print(args.source, " moved to ", args.destination)
      end,
    },
  },

  filesystem = {
    hide_root_node = true,
    retain_hidden_root_indent = true,
    filtered_items = {
      show_hidden_count = false,
      never_show = {
        ".DS_Store",
      },
    },
    window = {
      mappings = {
        ["tf"] = "telescope_find",
        ["tg"] = "telescope_grep",
      },
    },
  },
  commands = {
    telescope_find = function(state)
      local node = state.tree:get_node()
      local path = node:get_id()
      require("telescope.builtin").find_files(getTelescopeOpts(state, path))
    end,
    telescope_grep = function(state)
      local node = state.tree:get_node()
      local path = node:get_id()
      require("telescope.builtin").live_grep(getTelescopeOpts(state, path))
    end,
  },
})

local wk = require("which-key")

wk.add({
  { "<leader>on", "<cmd>Neotree<CR>", desc = "Open Side Explorer", mode = { "n", "i" } },
})
