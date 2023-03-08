local mark = require("harpoon.mark")
local ui = require("harpoon.ui")


local wk = require("which-key")
-- local opts_n = require("drystuff.noremaps").map_options("n")
-- local opts_v = require("drystuff.noremaps").map_options("v")

local mappings = {
        a = {
                name = "Buffer Navigation",
                h = { mark.add_file, "Add Buffer to Harpoon Stack" },
                --r = { "<cmd>Gitsigns reset_hunk<CR>", "Reset Hunk" },
                --a = { "<cmd>Gitsigns stage_buffer<CR>", "Stage Buffer" },
                --A = { "<cmd>Gitsigns undo_stage_hunk<CR>", "Undo Stage Buffer" },
                --R = { "<cmd>Gitsigns reset_buffer<CR>", "Reset Buffer" },
                -- p = {},
                --b = { "<cmd>lua gs.toggle_current_line_blame<CR>", "Toggle curret line blame" },
                -- d = {},
        }
}

wk.register(mappings, { prefix = "<leader>" })


-- vim.keymap.set("n", "<leader>h", mark.add_file)
vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu)
vim.keymap.set("n", "<1>", function() ui.nav_file(1) end)
vim.keymap.set("n", "<2>", function() ui.nav_file(2) end)
vim.keymap.set("n", "<3>", function() ui.nav_file(3) end)
vim.keymap.set("n", "<4>", function() ui.nav_file(4) end)
