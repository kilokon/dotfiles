-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
        -- Packer can manage itself
        use 'wbthomason/packer.nvim'
        -- Post-install/update hook with neovim command
        --  use { 'nvim-treesitter/nvim-treesitter' }

        -- ui elements
        use 'stevearc/dressing.nvim'

        -- Common Requirement
        use 'nvim-lua/plenary.nvim'
        use 'kyazdani42/nvim-web-devicons'
        use 'nvim-lua/popup.nvim'
        use 'mattn/webapi-vim'
        use { 'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async' }
        -- Search and Destroy
        -- use 'windwp/nvim-spectre'

        -- Nvim Telescope__
        use {
                'nvim-telescope/telescope.nvim',
                tag = '0.1.1',
                -- or                            , branch = '0.1.x',
                requires = { { 'nvim-lua/plenary.nvim' } },
        }
        use { 'nvim-telescope/telescope-ui-select.nvim' }
        use {
                'nvim-telescope/telescope-frecency.nvim',
                -- config = function()
                --         require "telescope".load_extension("frecency")
                -- end,
                requires = { 'kkharji/sqlite.lua' },
        }
        use { 'nvim-telescope/telescope-file-browser.nvim' }
        -- Telescope Extensions
        use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
        use 'jvgrootveld/telescope-zoxide' -- Telescope Zoxide

        --Navigation and Movement__

        --tab/buffer/list
        use 'liangxianzhe/nap.nvim'

        -- Mini independent Lua modules . The Swiss Army knife__
        use 'echasnovski/mini.nvim'


        -- Leap & flit (f/F/t/T motions on steroids, building on the Leap interface.)__
        use 'ggandor/leap.nvim'
        use 'ggandor/flit.nvim'

        -- Save Nvim Sessions__
        use 'stevearc/resession.nvim'

        --Buffer Navigation
        use 'theprimeagen/harpoon'

        -- Colors
        use 'folke/tokyonight.nvim'
        use 'rebelot/kanagawa.nvim'

        -- shade active inactive window
        -- -- use 'sunjon/Shade.nvim'
        --
        -- Status Line
        use { 'nvim-lualine/lualine.nvim' }
        use {
                'SmiteshP/nvim-navic',
                -- requires = "neovim/nvim-lspconfig"
        }

        --  indentation guides
        -- use 'lukas-reineke/indent-blankline.nvim'

        -- Buffer Line
        -- use 'noib3/nvim-cokeline'

        -- Treesitter
        use {
                'nvim-treesitter/nvim-treesitter',
                run = ':TSUpdate',
                --    config = config("treesitter"),
        }
        use 'nvim-treesitter/playground'
        use 'nvim-treesitter/nvim-treesitter-textobjects'
        use 'nvim-treesitter/nvim-treesitter-refactor'
        use 'TornaxO7/tree-setter'
        use 'David-Kunz/markid'
        use "IndianBoy42/tree-sitter-just" -- justfile Parser


        -- LSP , CMP, SNIPPETS, MASON
        use {
                'VonHeikemen/lsp-zero.nvim',
                requires = {
                        -- LSP Support
                        { 'neovim/nvim-lspconfig' },
                        { 'williamboman/mason.nvim' },
                        { 'williamboman/mason-lspconfig.nvim' },

                        -- Autocompletion
                        { 'hrsh7th/nvim-cmp' },
                        { 'hrsh7th/cmp-buffer' },
                        { 'hrsh7th/cmp-path' },
                        { 'saadparwaiz1/cmp_luasnip' },
                        { 'hrsh7th/cmp-nvim-lsp' },
                        { 'hrsh7th/cmp-nvim-lua' },

                        -- Snippets
                        { 'L3MON4D3/LuaSnip' },
                        -- Snippet Collection (Optional)
                        { 'rafamadriz/friendly-snippets' },
                },
        }

        -- LSP Saga
        -- use { 'glepnir/lspsaga.nvim', opt = true, event = 'BufRead' }

        -- Use LSP Diagonastics
        -- use 'chikko80/error-lens.nvim'

        -- Linter and formatter
        use 'jose-elias-alvarez/null-ls.nvim'

        -- REPL MANAGER
        use { 'hkupty/iron.nvim' }
        --UI for nvim-lsp progress
        use { 'j-hui/fidget.nvim',
                tag = 'legacy'
        }

        -- Diagonastics list and troubleshoot
        use 'folke/trouble.nvim'

        -- inlay-hints
        use 'simrat39/inlay-hints.nvim'

        -- A code outline window for skimming and quick navigation
        -- use 'stevearc/aerial.nvim'


        -- Debugging Code
        use 'mfussenegger/nvim-dap'
        use 'rcarriga/nvim-dap-ui'
        use 'theHamsta/nvim-dap-virtual-text'
        use 'nvim-telescope/telescope-dap.nvim'
        use 'mfussenegger/nvim-dap-python'
        -- use 'Pocco81/DAPInstall.nvim'


        -- Lua
        -- use 'folke/neodev.nvim'

        -- Rust
        use 'simrat39/rust-tools.nvim'
        use 'Canop/nvim-bacon'
        use 'Saecki/crates.nvim'

        -- Haskell
        use {
                'mrcjkb/haskell-tools.nvim',
                requires = {
                        'nvim-lua/plenary.nvim',
                        'nvim-telescope/telescope.nvim', -- optional
                },
                -- branch = '1.10.1',                       -- recommended
        }

        -- Typst
        use { 'kaarmu/typst.vim', ft = { 'typst' } }

        -- GlSl Viewer
        use {
                'timtro/glslView-nvim',
                ft = 'glsl',
                config = function()
                        require('glslView').setup {
                                exe_path = '/usr/local/bin/glslViewer',
                                arguments = { '-l', '-w', '128', '-h', '256' },
                        }
                end,
        }

        --superCollider
        use {
                'davidgranstrom/scnvim',
                config = function()
                        require('scnvim').setup()
                end,
        }

        -- NU Shell
        use {
                'LhKipp/nvim-nu',
                run = ':TSInstall nu'
        }

        -- Comments
        use {
                'numToStr/Comment.nvim',

                -- config = function()
                --         require('Comment').setup()
                -- end,
        }

        use 'JoosepAlviste/nvim-ts-context-commentstring'



        -- parens
        --use "windwp/nvim-autopairs"

        -- Git workflow
        use {
                'lewis6991/gitsigns.nvim',
                -- tag = 'release' -- To use the latest release (do not use this if you run Neovim nightly or dev builds!)
        }

        -- Terminals
        use 'akinsho/toggleterm.nvim'

        use('mrjones2014/smart-splits.nvim')

        -- vim learning
        -- use 'ThePrimeagen/vim-be-good'

        -- Speed up loading Lua modules in Neovim to improve startup time.
        use 'lewis6991/impatient.nvim'



        -- Which Key
        use {
                'folke/which-key.nvim',
        }
        use({
                "utilyre/barbecue.nvim",
                tag = "*",
                requires = {
                        "SmiteshP/nvim-navic",
                        "nvim-tree/nvim-web-devicons", -- optional dependency
                },
        })

        -- use({
        --         "utilyre/barbecue.nvim",
        --         tag = "*",
        --         requires = {
        --                 "SmiteshP/nvim-navic",
        --                 "nvim-tree/nvim-web-devicons", -- optional dependency
        --         },
        --         after = "nvim-web-devicons",           -- keep this if you're using NvChad
        -- })
        use {
                'JellyApple102/flote.nvim',
                --                config = function()
                --                        require('flote').setup {
                --                                -- your configuration comes here
                --                                -- or leave it empty to use the default settings
                --                                -- refer to the configuration section below
                --                        }
                --                end,
        }

        use {
                "nvim-neo-tree/neo-tree.nvim",
                branch = "v2.x",
                requires = {
                        "nvim-lua/plenary.nvim",
                        "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
                        "MunifTanjim/nui.nvim",
                        {
                                -- only needed if you want to use the commands with "_with_window_picker" suffix
                                's1n7ax/nvim-window-picker',
                                tag = "v1.*",
                                config = function()
                                        require 'window-picker'.setup({
                                                autoselect_one = true,
                                                include_current = false,
                                                filter_rules = {
                                                        -- filter using buffer options
                                                        bo = {
                                                                -- if the file type is one of following, the window will be ignored
                                                                filetype = { 'neo-tree', "neo-tree-popup", "notify" },

                                                                -- if the buffer type is one of following, the window will be ignored
                                                                buftype = { 'terminal', "quickfix" },
                                                        },
                                                },
                                                other_win_hl_color = '#e35e4f',
                                        })
                                end,
                        }
                },
        }
end)
