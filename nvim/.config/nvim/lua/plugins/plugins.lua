-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
        -- Packer can manage itself
        use 'wbthomason/packer.nvim'
        -- Post-install/update hook with neovim command
        --  use { 'nvim-treesitter/nvim-treesitter' }


        -- Common Requirement
        use 'nvim-lua/plenary.nvim'
        use 'kyazdani42/nvim-web-devicons'
        use 'nvim-lua/popup.nvim'
        use 'mattn/webapi-vim'

        -- Search and Destroy
        -- use 'windwp/nvim-spectre'

        -- Nvim Telescope
        use {
                'nvim-telescope/telescope.nvim', tag = '0.1.1',
                -- or                            , branch = '0.1.x',
                requires = { { 'nvim-lua/plenary.nvim' } }
        }
        use { 'nvim-telescope/telescope-ui-select.nvim' }
        use {
                "nvim-telescope/telescope-frecency.nvim",
                -- config = function()
                --         require "telescope".load_extension("frecency")
                -- end,
                requires = { "kkharji/sqlite.lua" }
        }
        use { "nvim-telescope/telescope-file-browser.nvim" }
        -- Telescope Extensions
        use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
        use 'jvgrootveld/telescope-zoxide' -- Telescope Zoxide

        -- Mini independent Lua modules . The Swiss Army knife
        use 'echasnovski/mini.nvim'

        -- Comments
        use 'JoosepAlviste/nvim-ts-context-commentstring'

        -- Leap & flit (f/F/t/T motions on steroids, building on the Leap interface.)
        use "ggandor/leap.nvim"
        use "ggandor/flit.nvim"

        -- Save Nvim Sessions
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
        -- use { 'nvim-lualine/lualine.nvim' }
        use {
                "SmiteshP/nvim-navic",
                -- requires = "neovim/nvim-lspconfig"
        }

        --  indentation guides
        use "lukas-reineke/indent-blankline.nvim"

        -- Buffer Line
        use 'noib3/nvim-cokeline'



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

        -- Use LSP Diagonastics
        use 'jose-elias-alvarez/null-ls.nvim'

        use({
                "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
                config = function()
                        require("lsp_lines").setup()
                end,
        })

        -- FZF CMP
        -- use { 'tzachar/cmp-fuzzy-buffer', requires = { 'tzachar/fuzzy.nvim' } }

        -- Formatting & Linting
        use "lukas-reineke/lsp-format.nvim"
        -- use 'ttibsi/pre-commit.nvim'

        --UI for nvim-lsp progress
        use 'j-hui/fidget.nvim'

        -- Diagonastics list and troubleshoot
        use 'folke/trouble.nvim'

        -- inlay-hints
        use 'simrat39/inlay-hints.nvim'

        -- A code outline window for skimming and quick navigation
        use 'stevearc/aerial.nvim'

        -- use 'jose-elias-alvarez/null-ls.nvim'

        -- Debugging Code
        use 'mfussenegger/nvim-dap'

        -- Lua
        use "folke/neodev.nvim"

        -- Rust
        use 'simrat39/rust-tools.nvim'
        use 'Canop/nvim-bacon'
        use 'Saecki/crates.nvim'

        --superCollider
        use {
                'davidgranstrom/scnvim',
                config = function()
                        require('scnvim').setup()
                end
        }

        -- Commenting
        -- use {
        --         'numToStr/Comment.nvim',
        --         config = function()
        --                 require('Comment').setup()
        --         end
        -- }

        -- parens
        --use "windwp/nvim-autopairs"

        -- Git workflow
        use {
                'lewis6991/gitsigns.nvim',
                -- tag = 'release' -- To use the latest release (do not use this if you run Neovim nightly or dev builds!)
        }

        -- Terminals
        use 'akinsho/toggleterm.nvim'

        -- vim learning
        use 'ThePrimeagen/vim-be-good'

        -- Speed up loading Lua modules in Neovim to improve startup time.
        use 'lewis6991/impatient.nvim'

        --Nu Shell
        use 'LhKipp/nvim-nu'

        -- Which Key
        use {
                "folke/which-key.nvim",
                config = function()
                        vim.o.timeout = true
                        vim.o.timeoutlen = 300
                        require("which-key").setup {
                                -- your configuration comes here
                                -- or leave it empty to use the default settings
                                -- refer to the configuration section below
                        }
                end
        }
end)
