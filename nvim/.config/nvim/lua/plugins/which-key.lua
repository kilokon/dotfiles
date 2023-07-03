local wk = require('which-key')
local opts_n = require('drystuff.noremaps').map_options 'n'


wk.setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
}
-- local opts = { noremap = true, silent = true }

local mappings = {
        -- ["w"] = { "<cmd>w!<CR>", "Save" },
        -- ["/"] = { "<Plug>(comment_toggle_linewise_current)", "Comment toggle current line" },
        -- ["q"] = { "<cmd>BufferKill<CR>", "Close Buffer" },
        -- ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },

        -- ["e"] = { "<cmd>Ex<CR>", "Explorer" },

        ["\\"] = { "<cmd>:Neotree toggle<CR>", "Neotree" },
        -- e = {
        --         name = 'Netrw Mapping',
        --         x = { '<cmd>:Lexplore %:p:h<cr>', 'Open Explorer' },
        -- },
        U = {
                name = 'Updates',
                p = { '<cmd>PackerSync<cr>', 'PackerSync' },
                t = { '<cmd>TSUpdate<cr>', 'TreeSitter Update' },
        },
}


wk.register(mappings, opts_n)
