return {
  {
    "mfussenegger/nvim-lint",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      vim.env.ESLINT_D_PPID = vim.fn.getpid()
      local lint = require("lint")
      lint.linters_by_ft = {
        txt = { "cspell" },
        -- markdown = { "vale" },
        cpp = { "clangtidy", "cpplint" },
        bash = { "bash" },
        fish = { "fish" },
        javascript = { "eslint" },
        typescript = { "eslint" },
        -- javascript = { "eslint_d" },
        -- typescript = { "eslint_d" },
        -- javascript = { "eslint_d" },
        -- typescript = { "eslint" },
        -- javascriptreact = { "eslint" },
        -- typescriptreact = { "eslint" },
        python = { "ruff" },
      }
      local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

      vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
        group = lint_augroup,
        callback = function()
          lint.try_lint()
        end,
      })
      lint.linters.cspell = require("lint.util").wrap(lint.linters.cspell, function(diagnostic)
        diagnostic.severity = vim.diagnostic.severity.HINT
        return diagnostic
      end)
      -- vim.keymap.set("n", "<leader>l", function()
      --   require("lint").try_lint()
      -- end, { desc = "Trigger linting for current file" })
    end,
  },
  { "esmuellert/nvim-eslint" },
}
