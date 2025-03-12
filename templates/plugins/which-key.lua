local wk = require("which-key")

wk.setup({})

wk.add({
  { "<leader>t", group = "Terminal" }, --group
  { "<leader>f", group = "Telescope" },
  { "<leader>o", group = "Open" },
  { "<f5>", "<cmd>Neotree<CR>", desc = "Open Neotree", mode = { "i", "n" } },
  { "<f11>", "<cmd>Neogit<CR>", desc = "Open Neogit", mode = { "i", "n" } },
  -- b = { name = "Buffers" },
  -- g = { name = "Git" },
  -- l = { name = "LSP" },
  -- ld = { name = "Diagnostics" },
  -- lg = { name = "Goto" },
  -- --    n = { name = "Neorg" },
  -- no = { name = "ToC" },
  -- nm = { name = "Metadata" },
  -- p = { name = "Plugins" },
  -- s = { name = "Sessions" },
  -- t = { name = "Toggle" },
  -- w = { name = "Windows" },
})
