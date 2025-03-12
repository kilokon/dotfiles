require("rocks-config").configure("blink.cmp")
vim.cmd.packadd("blink.cmp")
require("blink.cmp").setup({
  keymap = {
    preset = "enter",
    ["<Up>"] = {},
    ["<Down>"] = {},
    ["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
    ["<C-e>"] = { "hide", "fallback" },
    ["<CR>"] = { "accept", "fallback" },
    ["<C-k>"] = { "show_signature", "hide_signature", "fallback" },
    ["<C-b>"] = { "scroll_documentation_up", "fallback" },
    ["<C-f>"] = { "scroll_documentation_down", "fallback" },
    ["<Tab>"] = {
      "select_next",
      -- function(cmp)
      --     if cmp.snippet_active() then
      --         return cmp.accept()
      --     else
      --         return cmp.select_next()
      --     end
      -- end,
      "snippet_forward",
      "fallback",
    },
    ["<S-Tab>"] = {
      "select_prev",
      -- function(cmp)
      --     if cmp.snippet_active() then
      --         return cmp.accept()
      --     else
      --         return cmp.select_prev()
      --     end
      -- end,
      "snippet_backward",
      "fallback",
    },
    -- cmdline = {
    --     preset = "super-tab",
    --     -- sets <CR> to accept the item and run the command immediately
    --     -- use `select_accept_and_enter` to accept the item or the first item if none are selected
    --     ["<Tab>"] = {
    --         "select_next",
    --         "fallback",
    --     },
    --     ["<S-Tab>"] = {
    --         "select_prev",
    --         "fallback",
    --     },
    --     ["<Up>"] = {},
    --     ["<Down>"] = {},
    --     ["<CR>"] = { "accept_and_enter", "fallback" },
    -- },
  },
  snippets = { preset = "luasnip" },
  cmdline = { enabled = false },
  sources = {
    default = { "lazydev", "lsp", "path", "snippets", "buffer" },
    providers = {
      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        -- make lazydev completions top priority (see `:h blink.cmp`)
        score_offset = 100,
      },
    },
    -- providers = {
    --     codecompanion = {
    --         name = "CodeCompanion",
    --         module = "codecompanion.providers.completion.blink",
    --         enabled = true,
    --     },
    --     copilot = {
    --         name = "copilot",
    --         module = "blink-copilot",
    --     },
    -- },
  },
  completion = {
    keyword = {
      range = "prefix",
    },
    list = {
      selection = {
        preselect = false,
        auto_insert = true,
      },
      cycle = {
        from_bottom = true,
        from_top = true,
      },
    },
    accept = {
      create_undo_point = true,
      auto_brackets = {
        enabled = true,
        kind_resolution = {
          enabled = true,
        },
        semantic_token_resolution = {
          enabled = true,
        },
      },
    },

    menu = {
      border = "padded",
      draw = {
        padding = 1,
        gap = 1,
        treesitter = { "lsp" },
        columns = {
          { "kind_icon" },
          { "label", "label_description", gap = 1 },
          { "source_name" },
        },
      },
    },
    documentation = {
      auto_show = true,
      auto_show_delay_ms = 500,
      window = {
        border = "padded",
        scrollbar = true,
      },
    },
    ghost_text = {
      enabled = true,
    },
  },
  appearance = {
    -- kind_icons = {
    --     Copilot = "",
    -- },
  },
  -- fuzzy = { implementation = "prefer_rust_with_warning" },
})
