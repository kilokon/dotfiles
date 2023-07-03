local iron = require("iron.core")
local view = require("iron.view")
iron.setup {
        config = {
                -- Whether a repl should be discarded or not
                scratch_repl = true,
                should_map_plug = false,
                highlight_last = false,
                -- Your repl definitions come here
                repl_definition = {
                        sh = {
                                -- Can be a table or a function that
                                -- returns a table (see below)
                                command = { "zsh" }
                        },
                        python = require("iron.fts.python").ipython,
                        rust = {
                                command = function(meta)
                                        local filename = vim.api.nvim_buf_get_name(meta.current_bufnr)
                                        return { 'irust', 'filename' }
                                end
                        },
                        haskell = {
                                command = function(meta)
                                        local file = vim.api.nvim_buf_get_name(meta.current_bufnr)
                                        -- call `require` in case iron is set up before haskell-tools
                                        return require('haskell-tools').repl.mk_repl_cmd(file)
                                end,
                        },
                        -- haskell = {
                        --         command = function(meta)
                        --                 local filename = vim.api.nvim_buf_get_name(meta.current_bufnr)
                        --                 return { 'cabal', 'v2-repl', filename }
                        --         end
                        -- },
                },
                -- How the repl window will be displayed
                -- See below for more information
                -- repl_open_cmd = require('iron.view').bottom(40),
                repl_open_cmd = view.split.vertical.botright(0.47)
                -- repl_open_cmd = "vertical botright 45 split",
        },
        -- Iron doesn't set keymaps by default anymore.
        -- You can set them here or manually add keymaps to the functions in iron.core
        keymaps = {
                send_motion = "<space>sc",
                visual_send = "<space>sc",
                send_file = "<space>sf",
                send_line = "<space>sl",
                send_mark = "<space>sm",
                mark_motion = "<space>mc",
                mark_visual = "<space>mc",
                remove_mark = "<space>md",
                cr = "<space>s<cr>",
                interrupt = "<space>s<space>",
                exit = "<space>sq",
                clear = "<space>cl",
        },
        -- If the highlight is on, you can change how it looks
        -- For the available options, check nvim_set_hl
        highlight = {
                italic = true
        },
        ignore_blank_lines = true, -- ignore blank lines when sending visual select lines
}

-- iron also has a list of commands, see :h iron-commands for all available commands
vim.keymap.set('n', '<space>rs', '<cmd>IronRepl<cr>')
vim.keymap.set('n', '<space>rr', '<cmd>IronRestart<cr>')
vim.keymap.set('n', '<space>rf', '<cmd>IronFocus<cr>')
vim.keymap.set('n', '<space>rh', '<cmd>IronHide<cr>')
