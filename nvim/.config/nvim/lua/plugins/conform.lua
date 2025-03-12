return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "isort", "black" },
      javascript = { "prettier", stop_after_first = true },
      ["c"] = { "clang-format" },
      ["c++"] = { "clang-format" },
      typst = { "typstyle" },
      json = { "prettier" },
      ["svg"] = { "xmlformatter" },
      ["xml"] = { "xmlformatter" },
      toml = { "taplo" },
    },
    format_on_save = {
      -- These options will be passed to conform.format()
      timeout_ms = 500,
      lsp_format = "fallback",
    },
  },
}
