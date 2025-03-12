return {
  {
    "OXY2DEV/markview.nvim",
    lazy = false,
    config = function()
      local mkv = require("markview")
      local spec = require("markview.spec")
      local presets = require("markview.presets").headings

      mkv.config = {

        experimental = {
          date_formats = {},
          date_time_formats = {},

          text_filetypes = {},
          read_chunk_size = 1000,
          link_open_alerts = false,
          file_open_command = "tabnew",

          list_empty_line_tolerance = 3,
        },
        highlight_groups = {},
        preview = {
          enable = true,
          filetypes = { "markdown", "quarto", "rmd", "typst" },
          ignore_buftypes = { "nofile" },
          ignore_previews = {},

          modes = { "n", "no", "c" },
          hybrid_modes = {},
          debounce = 50,
          draw_range = { vim.o.lines, vim.o.lines },
          edit_range = { 1, 0 },

          callbacks = {
            on_attach = function(_, wins)
              local attach_state = spec.get({ "preview", "enable" }, { fallback = true, ignore_enable = true })
              if attach_state == false then
                return
              end

              for _, win in ipairs(wins) do
                --- Preferred conceal level should
                --- be 3.
                vim.wo[win].conceallevel = 3
              end
            end,
          },
          splitview_winopts = {
            split = "right",
          },
        },
        renderers = {},

        html = {
          enable = true,

          container_elements = {},
          headings = {},
          void_elements = {},
        },
        latex = {
          enable = true,

          blocks = {},
          commands = {},
          escapes = {},
          fonts = {},
          inlines = {},
          parenthesis = {},
          subscripts = {},
          superscripts = {},
          symbols = {},
          texts = {},
        },
        markdown = {
          enable = true,

          block_quotes = {},
          code_blocks = {},
          headings = {},
          horizontal_rules = {},
          list_items = {},
          metadata_plus = {},
          metadata_minus = {},
          tables = {},
        },
        markdown_inline = {
          enable = true,

          block_references = {},
          checkboxes = {},
          emails = {},
          embed_files = {},
          entities = {},
          escapes = {},
          footnotes = {},
          highlights = {},
          hyperlinks = {},
          images = {},
          inline_codes = {},
          internal_links = {},
          uri_autolinks = {},
        },
        typst = {
          enable = true,

          codes = {},
          escapes = {},
          headings = {
            headings = presets.marker,
          },
          labels = {},
          list_items = {},
          math_blocks = {},
          math_spans = {},
          raw_blocks = {},
          raw_spans = {},
          reference_links = {},
          subscripts = {},
          superscript = {},
          symbols = {},
          terms = {},
          url_links = {},
        },
        yaml = {
          enable = true,

          properties = {},
        },
      }
    end,
  },

  -- {
  --   "iamcco/markdown-preview.nvim",
  --   cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
  --   build = "cd app && npm install",
  --   init = function()
  --     vim.g.mkdp_filetypes = { "markdown" }
  --   end,
  --   ft = { "markdown" },
  -- },
  -- {
  --   "MeanderingProgrammer/render-markdown.nvim",
  --   -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
  --   -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
  --   dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" }, -- if you prefer nvim-web-devicons
  --   opts = {},
  -- },
}
