return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = "VeryLazy",

  config = function()
    require("lualine").setup({
      options = {
        extensions = { "neo-tree", "nvim-dap-ui", "toggleterm", "overseer", "lazy" },
        fmt = string.lower,
        icons_enabled = true,
        theme = "auto",
        component_separators = { left = "", right = "" },
        section_separators = { left = "", right = "" },
        disabled_filetypes = {
          statusline = {
            "help",
            "neogitstatus",
            "neo-tree",
            "Trouble",
            "alpha",
            "Outline",
            "toggleterm",
            "qf",
          },
          tabline = {
            "help",
            "neogitstatus",
            "neo-tree",
            "Trouble",
            "Outline",
            "toggleterm",
            "qf",
          },
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
          statusline = 100,
          tabline = 100,
          winbar = 100,
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
        lualine_c = {
          -- {
          --   "filetype",
          --   icon_only = true,
          --   padding = { left = 1, right = 0 },
          -- },
          -- "filename",
          {
            "navic",
            icon = ">",
            padding = 0,
          },
          -- {
          --   function()
          --     return navic.get_location()
          --   end,
          --   cond = function()
          --     return navic.is_available()
          --   end,
          -- },
          -- "filename",
        },
        -- lualine_c = { { navic.get_location, cond = navic.is_available }, "filename" },
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
            -- symbols = { error = "E", warn = "W", info = "I", hint = "H" },
            symbols = { error = "✖ ", warn = "❢ ", info = "𝓲 ", hint = " " },
            padding = { left = 0, right = 1 },
            colored = true, -- Displays diagnostics status in color if set to true.
            update_in_insert = false, -- Update diagnostics in insert mode.
            always_visible = false, -- Show diagnostics even if there are none.
          },
        },
        lualine_y = { "location" },
        lualine_z = { "filetype" },
        -- lualine_y = {},
        -- lualine_z = { { "buffers", mode = 3 } },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { "filename" },
        lualine_x = { "location" },
        lualine_y = {},
        lualine_z = {},
      },
      tabline = {
        lualine_a = { "buffers" },
        lualine_z = { "tabs" },
      },
      winbar = {
        lualine_c = {
          -- {
          --   "navic",
          --   color_correction = nil,
          --   navic_opts = nil,
          -- },
        },
        -- lualine_z = {'filename'},
      },
      -- winbar = {},
      inactive_winbar = {},
      extensions = {},
    })
  end,
}
