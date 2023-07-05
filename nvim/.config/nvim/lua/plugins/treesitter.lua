-- local m = require 'markid'
-- local configs = require 'nvim-treesitter.configs'


require('nvim-treesitter.configs').setup {
        -- A list of parser names, or "all" (the four listed parsers should always be installed)
        ensure_installed = { 'c', 'lua', 'vim', 'vimdoc', 'rust', 'html' },

        -- Install parsers synchronously (only applied to `ensure_installed`)
        sync_install = false,

        -- Automatically install missing parsers when entering buffer
        -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
        auto_install = true,

        -- List of parsers to ignore installing (for "all")
        -- ignore_install = { "javascript" },

        ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
        -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

        highlight = {
                -- `false` will disable the whole extension
                enable = true,
                disable = {}, -- list of language that will be disabled
                -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
                -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
                -- the name of the parser)
                -- list of language that will be disabled
                -- disable = { "c", "rust" },
                -- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
                -- disable = function(lang, buf)
                --         local max_filesize = 100 * 1024 -- 100 KB
                --         local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
                --         if ok and stats and stats.size > max_filesize then
                --                 return true
                --         end
                -- end,
                -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
                -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
                -- Using this option may slow down your editor, and you may see some duplicate highlights.
                -- Instead of true it can also be a list of languages
                additional_vim_regex_highlighting = false,
        },
        refactor = {
                highlight_definitions = {
                        enable = true,
                        -- Set to false if you have an `updatetime` of ~100.
                        clear_on_cursor_move = true,
                },
        },
        textobjects = {
                select = {
                        enable = true,
                        -- Automatically jump forward to textobj, similar to targets.vim
                        lookahead = true,
                        keymaps = {
                                -- You can use the capture groups defined in textobjects.scm
                                ['af'] = '@function.outer',
                                ['if'] = '@function.inner',
                                ['ac'] = '@class.outer',
                                -- You can optionally set descriptions to the mappings (used in the desc parameter of
                                -- nvim_buf_set_keymap) which plugins like which-key display
                                ['ic'] = { query = '@class.inner', desc = 'Select inner part of a class region' },
                        },
                        -- You can choose the select mode (default is charwise 'v')
                        --
                        -- Can also be a function which gets passed a table with the keys
                        -- * query_string: eg '@function.inner'
                        -- * method: eg 'v' or 'o'
                        -- and should return the mode ('v', 'V', or '<c-v>') or a table
                        -- mapping query_strings to modes.
                        selection_modes = {
                                ['@parameter.outer'] = 'v', -- charwise
                                ['@function.outer'] = 'V',  -- linewise
                                ['@class.outer'] = '<c-v>', -- blockwise
                        },
                        -- If you set this to `true` (default is `false`) then any textobject is
                        -- extended to include preceding or succeeding whitespace. Succeeding
                        -- whitespace has priority in order to act similarly to eg the built-in
                        -- `ap`.
                        --
                        -- Can also be a function which gets passed a table with the keys
                        -- * query_string: eg '@function.inner'
                        -- * selection_mode: eg 'v'
                        -- and should return true of false
                        include_surrounding_whitespace = true,
                },
                tree_setter = {
                        enable = true,
                },
        },
        rainbow = {
                enable = true,
                extended_mode = true,
                max_file_lines = nil,
        },
        context_commentstring = {
                enable = true,
                config = {
                        javascript = {
                                __default = '// %s',
                                jsx_element = '{/* %s */}',
                                jsx_fragment = '{/* %s */}',
                                jsx_attribute = '// %s',
                                comment = '// %s',
                        },
                        haskell = {
                                __default = '-- %s',
                                __multiline = '{- %s -}',
                                comment = '-- %s',
                        },
                        rust = {
                                __default = '// %s',
                                __multiline = '/* %s */',
                                comment = '// %s',
                        },
                },
        },
        playground = {
                enable = true,
                disable = {},
                updatetime = 25,         -- Debounced time for highlighting nodes in the playground from source code
                persist_queries = false, -- Whether the query persists across vim sessions
                keybindings = {
                        toggle_query_editor = 'o',
                        toggle_hl_groups = 'i',
                        toggle_injected_languages = 't',
                        toggle_anonymous_nodes = 'a',
                        toggle_language_display = 'I',
                        focus_language = 'f',
                        unfocus_language = 'F',
                        update = 'R',
                        goto_node = '<cr>',
                        show_help = '?',
                },
        },
        markid = { enable = true }
        -- markid = {
        --         enable = true,
        --         colors = m.colors.medium,
        --         queries = m.queries,
        --         is_supported = function(bufnr, lang)
        --                 local config = configs.get_module("markid")
        --
        --                 local query = vim.treesitter.parse_query(lang, config.queries[lang] or config.queries["default"])
        --                 local parser = vim.treesitter.get_parser(bufnr, lang)
        --                 local tree = parser:parse()[1]
        --                 local root = tree:root()
        --
        --
        --                 --return pcall(root:query_iter_prepared(query))
        --                 --configs.get_module("markid").queries
        --                 return pcall(vim.treesitter.query.parse(), lang, queries[lang] or queries['default'])
        --         end
        -- }
}

--Additional Parser Configs
--Nu Shell

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

-- Nu Script Parser
parser_config.nu = {
        install_info = {
                url = "https://github.com/nushell/tree-sitter-nu",
                files = { "src/parser.c" },
                branch = "main",
        },
        filetype = "nu",
}

-- Just File Parser
parser_config.just = {
        install_info = {
                url = "https://github.com/IndianBoy42/tree-sitter-just", -- local path or git repo
                files = { "src/parser.c", "src/scanner.cc" },
                branch = "main",
                -- use_makefile = true -- this may be necessary on MacOS (try if you see compiler errors)
        },
        maintainers = { "@IndianBoy42" },
}


-- require "nvim-treesitter.install".compilers = { "gcc-11" }
