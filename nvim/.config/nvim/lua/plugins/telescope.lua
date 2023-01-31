-- local tel = require('telescope')


--local actions = require 'telescope.actions'
local builtin = require 'telescope.builtin'
local z_utils = require("telescope._extensions.zoxide.utils")
local trouble = require("trouble.providers.telescope")

-- local themes = require 'telescope.themes'

--map("n", "<leader>bb", "<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown())<cr>",
--       opts)
--
require('telescope').setup {
        print("loaded telescope"),
        defaults = {
                -- Default configuration for telescope goes here:
                -- config_key = value,
                file_ignore_patterns = {
                        '.git/', 'node_modules/', '.npm/', '*[Cc]ache/', '*-cache*',
                        '.dropbox/', '.dropbox_trashed/', '*.py[co]', '*.sw?', '*~',
                        '*.sql', '*.tags*', '*.gemtags*', '*.csv', '*.tsv', '*.tmp*',
                        '*.old', '*.plist', '*.pdf', '*.log', '*.jpg', '*.jpeg', '*.png',
                        '*.tar.gz', '*.tar', '*.zip', '*.class', '*.pdb', '*.dll',
                        '*.dat', '*.mca', '__pycache__', '.mozilla/', '.electron/',
                        '.vpython-root/', '.gradle/', '.nuget/', '.cargo/', '.evernote/',
                        '.azure-functions-core-tools/', 'yay/', '.local/share/Trash/',
                        '.local/share/nvim/swap/', 'code%-other/'
                },
                path_display = { "smart" },

                mappings = {
                        i = {
                                -- map actions.which_key to <C-h> (default: <C-/>)
                                -- actions.which_key shows the mappings for your picker,
                                -- e.g. git_{create, delete, ...}_branch for the git_branches picker
                                -- ["<C-h>"] = "which_key"
                        },
                        n = {
                                ["<esc>"] = require("telescope.actions").close,
                        }
                }
        },
        pickers = {
                -- Default configuration for builtin pickers goes here:
                -- picker_name = {
                --   picker_config_key = value,
                --   ...
                -- }
                -- Now the picker_config_key will be applied every time you call this
                -- builtin picker
        },
        extensions = {
                -- Your extension configuration goes here:
                -- extension_name = {
                --   extension_config_key = value,
                -- }
                -- please take a look at the readme of the extension you want to configure
                ["ui-select"] = {
                        require("telescope.themes").get_dropdown {
                                -- even more opts
                        }

                        -- pseudo code / specification for writing custom displays, like the one
                        -- for "codeactions"
                        -- specific_opts = {
                        --   [kind] = {
                        --     make_indexed = function(items) -> indexed_items, width,
                        --     make_displayer = function(widths) -> displayer
                        --     make_display = function(displayer) -> function(e)
                        --     make_ordinal = function(e) -> string
                        --   },
                        --   -- for example to disable the custom builtin "codeactions" display
                        --      do the following
                        --   codeactions = false,
                        -- }
                },
                file_browser = {
                        theme = "ivy",
                        -- disables netrw and use telescope-file-browser in its place
                        hijack_netrw = true,
                        mappings = {
                                ["i"] = {
                                        ["<c-t>"] = trouble.open_with_trouble,
                                        -- your custom insert mode mappings
                                },
                                ["n"] = {
                                        ["<c-t>"] = trouble.open_with_trouble,
                                        -- your custom normal mode mappings
                                },
                        },
                },
                fzf = {
                        fuzzy = true, -- false will only do exact matching
                        override_generic_sorter = true, -- override the generic sorter
                        override_file_sorter = true, -- override the file sorter
                        case_mode = "smart_case", -- or "ignore_case" or "respect_case"
                        -- the default case_mode is "smart_case"
                },
                zoxide = {
                        prompt_title = "[ Directory ]",
                        mappings = {
                                default = {
                                        after_action = function(selection)
                                                print("Update to (" .. selection.z_score .. ") " .. selection.path)
                                        end
                                },
                                ["<C-s>"] = {
                                        before_action = function(selection) print("before C-s") end,
                                        action = function(selection)
                                                vim.cmd("edit " .. selection.path)
                                        end
                                },
                                -- Opens the selected entry in a new split
                                ["<C-q>"] = { action = z_utils.create_basic_command("split") },
                        },
                }
        }
}


-- Load the extensions
require('telescope').load_extension('fzf')
require('telescope').load_extension('zoxide')


local opts = { noremap = true, silent = true }
--" Using Lua functions
-- vim.api.nvim_set_keymap('n', '<leader>ff', ":lua builtin.live_grep<CR>", opts)
vim.keymap.set('n', '<C-p>', builtin.find_files, opts)
vim.keymap.set('n', '<leader>ff', builtin.live_grep, opts)
vim.keymap.set('n', '<leader>fb', builtin.buffers, opts)
vim.keymap.set('n', '<leader>fh', builtin.help_tags, opts)
-- vim.keymap.set("n", "<leader>cd", tel.extensions.zoxide.list)
