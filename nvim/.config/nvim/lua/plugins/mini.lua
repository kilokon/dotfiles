-- No need to copy this inside `setup()`. Will be used automatically.
-- require('mini.comment').setup {
--         -- Options which control module behavior
--         options = {
--                 -- Whether to ignore blank lines when adding comment
--                 ignore_blank_line = false,
--                 -- Whether to recognize as comment only lines without indent
--                 start_of_line = false,
--                 -- Whether to ensure single space pad for comment parts
--                 pad_comment_parts = true,
--         },
--         -- Module mappings. Use `''` (empty string) to disable one.
--         mappings = {
--                 -- Toggle comment (like `gcip` - comment inner paragraph) for both
--                 -- Normal and Visual modes
--                 comment = 'gc',
--                 -- Toggle comment on current line
--                 comment_line = 'gcc',
--                 -- Define 'comment' textobject (like `dgc` - delete whole comment block)
--                 textobject = 'gc',
--         },
--         -- Hook functions to be executed at certain stage of commenting
--         hooks = {
--                 -- Before successful commenting. Does nothing by default.
--                 pre = function()
--                         -- require('ts_context_commentstring.internal').update_commentstring {}
--                 end,
--                 -- After successful commenting. Does nothing by default.
--                 post = function()
--                 end,
--         },
-- }

require('mini.pairs').setup {
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
}

require('mini.indentscope').setup {
        symbol = '|',
}

require('mini.move').setup()

require('mini.surround').setup {
        -- Module mappings. Use `''` (empty string) to disable one.
        mappings = {
                add = 'sa',            -- Add surrounding in Normal and Visual modes
                delete = 'sd',         -- Delete surrounding
                find = 'sf',           -- Find surrounding (to the right)
                find_left = 'sF',      -- Find surrounding (to the left)
                highlight = 'sh',      -- Highlight surrounding
                replace = 'sr',        -- Replace surrounding
                update_n_lines = 'sn', -- Update `n_lines`
                suffix_last = 'l',     -- Suffix to search with "prev" method
                suffix_next = 'n',     -- Suffix to search with "next" method
        },
}
require('mini.ai').setup {
        -- Table with textobject id as fields, textobject specification as values.
        -- Also use this to disable builtin textobjects. See |MiniAi.config|.
        custom_textobjects = nil,

        -- Module mappings. Use `''` (empty string) to disable one.
        mappings = {
                -- Main textobject prefixes
                around = 'a',
                inside = 'i',
                -- Next/last variants
                around_next = 'an',
                inside_next = 'in',
                around_last = 'al',
                inside_last = 'il',
                -- Move cursor to corresponding edge of `a` textobject
                goto_left = 'g[',
                goto_right = 'g]',
        },

        -- Number of lines within which textobject is searched
        n_lines = 50,

        -- How to search for object (first inside current line, then inside
        -- neighborhood). One of 'cover', 'cover_or_next', 'cover_or_prev',
        -- 'cover_or_nearest', 'next', 'previous', 'nearest'.
        search_method = 'cover_or_next',

        -- Whether to disable showing non-error feedback
        silent = false,
}