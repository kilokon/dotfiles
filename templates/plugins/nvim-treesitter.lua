require("nvim-treesitter.configs").setup({

  ensure_installed = {
    "c",
    "bash",
    "css",
    "dockerfile",
    "gitignore",
    "html",
    "json",
    "javascript",
    "markdown",
    "markdown_inline",
    "typescript",
    "tsx",
    "vimdoc",
    "yaml",
  },
  auto_install = true,
  sync_install = true,
  ignore_install = {},
  modules = {},
  highlight = {
    enable = true,
  },
  matchup = {
    enable = true,
  },
  autotag = {
    enable = true,
  },
  endwise = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-space>",
      node_incremental = "<C-space>",
      scope_incremental = false,
      node_decremental = "<bs>",
    },
  },
  textobjects = {

    select = {
      enable = true,
      lookahead = true,

      keymaps = {
        -- Variable
        ["a="] = { query = "@assignment.outer", desc = "Select outer part of an assignment" },
        ["i="] = { query = "@assignment.inner", desc = "Select inner part of an assignment" },
        ["l="] = { query = "@assignment.lhs", desc = "Select left hand side of an assignment" },
        ["r="] = { query = "@assignment.rhs", desc = "Select right hand side of an assignment" },
        -- Classes
        ["ac"] = { query = "@class.outer", desc = "Select outer part of a class" },
        ["ic"] = { query = "@class.inner", desc = "Select inner part of a class" },
        -- Function
        ["af"] = { query = "@call.outer", desc = "Select outer part of a function call region" },
        ["if"] = { query = "@call.inner", desc = "Select inner part of a function call region" },
        -- Parameter
        ["aa"] = { query = "@parameter.outer", desc = "Select outer part of a parameter/argument" },
        ["ia"] = { query = "@parameter.inner", desc = "Select inner part of a parameter/argument" },
        -- Conditional
        ["ai"] = { query = "@conditional.outer", desc = "Select outer part of a conditional" },
        ["ii"] = { query = "@conditional.inner", desc = "Select inner part of a conditional" },
        -- Loop
        ["al"] = { query = "@loop.outer", desc = "Select outer part of a loop" },
        ["il"] = { query = "@loop.inner", desc = "Select inner part of a loop" },
        -- Methods
        ["am"] = {
          query = "@function.outer",
          desc = "Select outer part of a method/function definition",
        },
        ["im"] = {
          query = "@function.inner",
          desc = "Select inner part of a method/function definition",
        },
      },
      selection_modes = {
        ["@parameter.outer"] = "v", -- charwise
        ["@function.outer"] = "V", -- linewise
        ["@class.outer"] = "<c-v>", -- blockwise
      },
      include_surrounding_whitespace = true,
    },
    swap = {
      enable = true,
    },
    move = {
      enable = true,
    },
  },
})

vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
