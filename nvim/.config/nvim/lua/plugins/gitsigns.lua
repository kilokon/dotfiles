return {
  "lewis6991/gitsigns.nvim",
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    on_attach = function(bufnr)
      local gitsigns = require("gitsigns")

      local function map(mode, lhs, rhs)
        vim.keymap.set(mode, lhs, rhs, { buffer = bufnr })
      end

      map("n", "]h", function()
        gitsigns.nav_hunk("next")
      end)
      map("n", "[h", function()
        gitsigns.nav_hunk("prev")
      end)
      map("n", "<leader>hs", gitsigns.stage_hunk)
      map("n", "<leader>hr", gitsigns.reset_hunk)
      map("n", "<leader>hS", gitsigns.stage_buffer)
      map("n", "<leader>hR", gitsigns.reset_buffer)
      map("n", "<leader>hp", gitsigns.preview_hunk)
      map("n", "<leader>hb", gitsigns.blame_line)
      map("n", "<leader>hd", gitsigns.diffthis)
      map("n", "<leader>ub", gitsigns.toggle_current_line_blame)
      map({ "o", "x" }, "ih", ":<c-u>Gitsigns select_hunk<cr>")
    end,
  },
}
-- end
-- opts = {
--   signs = {
--     add = { text = "▎" },
--     change = { text = "▎" },
--     delete = { text = "" },
--     topdelete = { text = "" },
--     changedelete = { text = "▎" },
--     untracked = { text = "▎" },
--   },
--   signs_staged = {
--     add = { text = "▎" },
--     change = { text = "▎" },
--     delete = { text = "" },
--     topdelete = { text = "" },
--     changedelete = { text = "▎" },
--   },
-- }
--   on_attach = function(buffer)
--     local gs = package.loaded.gitsigns
--
--     local function map(mode, l, r, desc)
--       vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
--     end
--
--     -- stylua: ignore start
--     map("n", "]h", function()
--       if vim.wo.diff then
--         vim.cmd.normal({ "]c", bang = true })
--       else
--         gs.nav_hunk("next")
--       end
--     end, "Next Hunk")
--     map("n", "[h", function()
--       if vim.wo.diff then
--         vim.cmd.normal({ "[c", bang = true })
--       else
--         gs.nav_hunk("prev")
--       end
--     end, "Prev Hunk")
--     map("n", "]H", function() gs.nav_hunk("last") end, "Last Hunk")
--     map("n", "[H", function() gs.nav_hunk("first") end, "First Hunk")
--     map({ "n", "v" }, "<leader>ghs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
--     map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
--     map("n", "<leader>ghS", gs.stage_buffer, "Stage Buffer")
--     map("n", "<leader>ghu", gs.undo_stage_hunk, "Undo Stage Hunk")
--     map("n", "<leader>ghR", gs.reset_buffer, "Reset Buffer")
--     map("n", "<leader>ghp", gs.preview_hunk_inline, "Preview Hunk Inline")
--     map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame Line")
--     map("n", "<leader>ghB", function() gs.blame() end, "Blame Buffer")
--     map("n", "<leader>ghd", gs.diffthis, "Diff This")
--     map("n", "<leader>ghD", function() gs.diffthis("~") end, "Diff This ~")
--     map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
--   end,
-- },
-- }
