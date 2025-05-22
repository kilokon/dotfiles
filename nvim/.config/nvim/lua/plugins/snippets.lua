-- Read about vscode style snippets
-- https://github.com/chrisgrieser/nvim-scissors?tab=readme-ov-file#introduction-to-the-vscode-style-snippet-format

return {
  {
    "L3MON4D3/LuaSnip",
    event = "InsertEnter",
    version = "v2.*",
    build = "make install_jsregexp",
    config = function()
      local ls = require("luasnip")
      require("luasnip.loaders.from_vscode").lazy_load({
        -- paths = { vim.fn.stdpath("config") .. "/snippets" },
        paths = { "./snippets" },
      })
      -- ls.filetype_set("C", { "c" })
      ls.setup({
        -- ls.config.set_config({
        update_events = { "TextChanged", "TextChangedI" },
        delete_check_events = { "InsertLeave" },
        region_check_events = { "InsertEnter" },
        enable_autosnippets = true,
      })
    end,
  },
  {
    "chrisgrieser/nvim-scissors",
    dependencies = "nvim-telescope/telescope.nvim",
    opts = {
      snippetDir = vim.fn.stdpath("config") .. "/snippets",
    },
  },
}
