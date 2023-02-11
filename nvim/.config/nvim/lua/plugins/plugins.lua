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




        --Buffer Navigation
        use 'theprimeagen/harpoon'

        -- Colors
        use 'folke/tokyonight.nvim'

        -- Status Line
        use { 'nvim-lualine/lualine.nvim' }

        -- Tabline
        -- use "romgrk/barbar.nvim"

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

        -- FZF CMP
        use { 'tzachar/cmp-fuzzy-buffer', requires = { 'tzachar/fuzzy.nvim' } }

        -- Formatting & Linting
        -- use "lukas-reineke/lsp-format.nvim"
        -- use 'ttibsi/pre-commit.nvim'

        --UI for nvim-lsp progress
        use 'j-hui/fidget.nvim'

        -- Diagonastics list and troubleshoot
        use 'folke/trouble.nvim'

        -- use 'jose-elias-alvarez/null-ls.nvim'

        -- Debugging Code
        use 'mfussenegger/nvim-dap'
        -- rust
        use 'simrat39/rust-tools.nvim'

        --superCollider
        use {
                'davidgranstrom/scnvim',
                config = function()
                        require('scnvim').setup()
                end
        }

        -- Commenting
        use {
                'numToStr/Comment.nvim',
                config = function()
                        require('Comment').setup()
                end
        }

        -- parens
        use {
                "windwp/nvim-autopairs",
                config = function()
                        require("nvim-autopairs").setup {}
                end
        }

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
end)
