-- No need to copy this inside `setup()`. Will be used automatically.
require('mini.comment').setup({
        -- Options which control module behavior
        options = {
                -- Whether to ignore blank lines when adding comment
                ignore_blank_line = false,
                -- Whether to recognize as comment only lines without indent
                start_of_line = false,
                -- Whether to ensure single space pad for comment parts
                pad_comment_parts = true,
        },
        -- Module mappings. Use `''` (empty string) to disable one.
        mappings = {
                -- Toggle comment (like `gcip` - comment inner paragraph) for both
                -- Normal and Visual modes
                comment = 'gc',
                -- Toggle comment on current line
                comment_line = 'gcc',
                -- Define 'comment' textobject (like `dgc` - delete whole comment block)
                textobject = 'gc',
        },
        -- Hook functions to be executed at certain stage of commenting
        hooks = {
                -- Before successful commenting. Does nothing by default.
                pre = function()
                        require("ts_context_commentstring.internal").update_commentstring({})
                end,
                -- After successful commenting. Does nothing by default.
                post = function()
                end,
        },
})


require('mini.pairs').setup({
        -- In which modes mappings from this `config` should be created
        modes = { insert = true, command = false, terminal = false },
        -- Global mappings. Each right hand side should be a pair information, a
        -- table with at least these fields (see more in |MiniPairs.map|):
        -- - <action> - one of 'open', 'close', 'closeopen'.
        -- - <pair> - two character string for pair to be used.
        -- By default pair is not inserted after `\`, quotes are not recognized by
        -- `<CR>`, `'` does not insert pair after a letter.
        -- Only parts of tables can be tweaked (others will use these defaults).
        mappings = {
                ['('] = { action = 'open', pair = '()', neigh_pattern = '[^\\].' },
                ['['] = { action = 'open', pair = '[]', neigh_pattern = '[^\\].' },
                ['{'] = { action = 'open', pair = '{}', neigh_pattern = '[^\\].' },
                [')'] = { action = 'close', pair = '()', neigh_pattern = '[^\\].' },
                [']'] = { action = 'close', pair = '[]', neigh_pattern = '[^\\].' },
                ['}'] = { action = 'close', pair = '{}', neigh_pattern = '[^\\].' },
                ['<'] = { action = 'open', pair = '<>', neigh_pattern = '[^\\].' },
                ['>'] = { action = 'close', pair = '<>', neigh_pattern = '[^\\].' },
                ['"'] = { action = 'closeopen', pair = '""', neigh_pattern = '[^\\].', register = { cr = false } },
                ["'"] = { action = 'closeopen', pair = "''", neigh_pattern = '[^%a\\].', register = { cr = false } },
                ['`'] = { action = 'closeopen', pair = '``', neigh_pattern = '[^\\].', register = { cr = false } },
        },
})

require('mini.indentscope').setup({
        symbol = '|',
})

require('mini.move').setup()

require('mini.surround').setup({
        mappings = {
                add = "gza",    -- Add surrounding in Normal and Visual modes
                delete = "gzd", -- Delete surrounding
                find = "gzf",   -- Find surrounding (to the right)
                find_left = "gzF", -- Find surrounding (to the left)
                highlight = "gzh", -- Highliht surroundin
                replace = "gzr", -- Replace surrounding
                update_n_lines = "gzn", -- Update `n_lines`
        },
})
-- require('mini.cursorword').setup()
