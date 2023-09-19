return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      "onsails/lspkind-nvim",
      "SmiteshP/nvim-navic",
    },
    event = "VeryLazy",
    -- lazy = false,
    -- event = "UIEnter",
    -- priority = 999,
    config = function()
      local navic = require("nvim-navic")
      -- local icons = require("lazyvim.config").icons
      require("lualine").setup({
        options = {
          fmt = string.lower,
          icons_enabled = false,
          theme = "auto",
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          disabled_filetypes = {
            statusline = {},
            winbar = {},
          },
          ignore_focus = {},
          always_divide_middle = true,
          globalstatus = false,
          refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
          },
        },
        sections = {
          lualine_a = {
            {
              "mode",
              fmt = function(str)
                return str:sub(1, 1)
              end,
            },
          },
          lualine_b = { "branch" },
          lualine_c = { { navic.get_location, cond = navic.is_available }, "filename" },
          lualine_x = {
            -- 'diagnostics',
            {
              "diagnostics",
              -- symbols = {
              --   error = icons.diagnostics.Error,
              --   warn = icons.diagnostics.Warn,
              --   info = icons.diagnostics.Info,
              --   hint = icons.diagnostics.Hint,
              -- },
              -- Table of diagnostic sources, available sources are:
              --   'nvim_lsp', 'nvim_diagnostic', 'nvim_workspace_diagnostic', 'coc', 'ale', 'vim_lsp'.
              -- or a function that returns a table as such:
              --   { error=error_cnt, warn=warn_cnt, info=info_cnt, hint=hint_cnt }
              -- symbols = { Error = " ", Warn = " ", Hint = " ", Info = " " },
              sources = { "nvim_diagnostic", "nvim_lsp" },
              -- Displays diagnostics for the defined severity types
              sections = { "error", "warn", "info", "hint" },
              diagnostics_color = {
                -- Same values as the general color option can be used here.
                error = "DiagnosticError", -- Changes diagnostics' error color.
                warn = "DiagnosticWarn", -- Changes diagnostics' warn color.
                info = "DiagnosticInfo", -- Changes diagnostics' info color.
                hint = "DiagnosticHint", -- Changes diagnostics' hint color.
              },
              symbols = { error = "E", warn = "W", info = "I", hint = "H" },
              colored = true, -- Displays diagnostics status in color if set to true.
              update_in_insert = false, -- Update diagnostics in insert mode.
              always_visible = false, -- Show diagnostics even if there are none.
            },
          },
          lualine_y = { "location" },
          -- lualine_y = {},
          lualine_z = { "filetype" },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = { "location" },
          lualine_y = {},
          lualine_z = {},
        },
        tabline = {},
        winbar = {},
        inactive_winbar = {},
        extensions = {},
      })
    end,
  },
  {
    "stevearc/dressing.nvim",
    opts = {},
  },
  -- { "akinsho/bufferline.nvim", version = "*", dependencies = "nvim-tree/nvim-web-devicons" },
  {
    -- TODO: Mike, fork this and make it stable maybe?
    "edluffy/specs.nvim",
    config = function()
      local success, colors = pcall(require, "vscode.colors")
      local bg = colors and colors.Violet or "#646695"
      local fg = colors and colors.Gray or "#808080"
      vim.api.nvim_set_hl(0, "Specs", {
        fg = fg,
        bg = bg,
      })
      local specs = require("specs")
      specs.setup({
        show_jumps = true,
        popup = {
          delay_ms = 5,
          inc_ms = 10,
          width = 20,
          fader = specs.pulse_fader,
          resizer = specs.slide_resizer,
          winhl = "Specs",
        },
        -- ignore_filetypes = excluded_filetypes_array,
        ignore_buftypes = { nofile = true, prompt = true },
      })
    end,
  },
  { "lewis6991/satellite.nvim" },
  -- { "Bekaboo/dropbar.nvim" },
}
