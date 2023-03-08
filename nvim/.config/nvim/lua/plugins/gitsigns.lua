-- gitsigns --
local wk = require("which-key")
require("gitsigns").setup {
        signs = {
                add = { text = "+" },
                change = { text = "~" },
        },

        on_attach = function(bufnr)
                local gs = package.loaded.gitsigns

                local function map(mode, l, r, opts)
                        opts = opts or {}
                        opts.buffer = bufnr
                        vim.keymap.set(mode, l, r, opts)
                end

                local mappings = {
                        h = {
                                b = { gs.toggle_current_line_blame, "Toggle curret line blame" },
                        }
                }
                wk.register(mappings, { prefix = "<leader>" })
                -- Navigation
                map('n', ']c', function()
                        if vim.wo.diff then return ']c' end
                        vim.schedule(function() gs.next_hunk() end)
                        return '<Ignore>'
                end, { expr = true })

                map('n', '[c', function()
                        if vim.wo.diff then return '[c' end
                        vim.schedule(function() gs.prev_hunk() end)
                        return '<Ignore>'
                end, { expr = true })

                -- Actions
                -- map({ 'n', 'v' }, '<leader>hs', ':Gitsigns stage_hunk<CR>')
                -- map({ 'n', 'v' }, '<leader>hr', ':Gitsigns reset_hunk<CR>')
                -- map('n', '<leader>hS', gs.stage_buffer)
                -- map('n', '<leader>hu', gs.undo_stage_hunk)
                -- map('n', '<leader>hR', gs.reset_buffer)
                -- map('n', '<leader>hp', gs.preview_hunk)
                -- map('n', '<leader>hb', function() gs.blame_line { full = true } end)
                -- map('n', '<leader>tb', gs.toggle_current_line_blame)
                -- map('n', '<leader>hd', gs.diffthis)
                -- map('n', '<leader>hD', function() gs.diffthis('~') end)
                -- map('n', '<leader>td', gs.toggle_deleted)

                -- Text object
                -- map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
        end

}
-- local opts_n = require("drystuff.noremaps").map_options("n")
-- local opts_v = require("drystuff.noremaps").map_options("v")

local mappings = {
        h = {
                name = "Git",
                s = { "<cmd>Gitsigns stage_hunk<CR>", "Stage Hunk" },
                r = { "<cmd>Gitsigns reset_hunk<CR>", "Reset Hunk" },
                a = { "<cmd>Gitsigns stage_buffer<CR>", "Stage Buffer" },
                A = { "<cmd>Gitsigns undo_stage_hunk<CR>", "Undo Stage Buffer" },
                R = { "<cmd>Gitsigns reset_buffer<CR>", "Reset Buffer" },
                -- p = {},

                -- d = {},
        }
}

wk.register(mappings, { prefix = "<leader>" })
