-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
        -- Packer can manage itself
        use 'wbthomason/packer.nvim'
        -- Post-install/update hook with neovim command
        --  use { 'nvim-treesitter/nvim-treesitter' }
        -- treesitter
        use {
                'nvim-treesitter/nvim-treesitter',
                run = ':TSUpdate',
                --    config = config("treesitter"),
        }
        use 'nvim-treesitter/playground'
        use 'nvim-treesitter/nvim-treesitter-textobjects'
        use 'nvim-treesitter/nvim-treesitter-refactor'

        use 'nvim-lua/plenary.nvim'
        use 'nvim-lua/popup.nvim'
        use {
                'nvim-telescope/telescope.nvim',
                tag = '0.1.0',
                -- or                            , branch = '0.1.x',
                requires = { { 'nvim-lua/plenary.nvim' } },
        }

        use 'kyazdani42/nvim-web-devicons'

        use 'folke/tokyonight.nvim'

        use { 'nvim-lualine/lualine.nvim' }
        -- Search and Destroy
        use 'windwp/nvim-spectre'
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

        -- Formatting
        use "lukas-reineke/lsp-format.nvim"

        --UI for nvim-lsp progress
        use 'j-hui/fidget.nvim'

        -- use 'jose-elias-alvarez/null-ls.nvim'

        -- rust
        use 'simrat39/rust-tools.nvim'

        --
        use {
                'numToStr/Comment.nvim',
                config = function()
                        require('Comment').setup()
                end
        }

        use {
                "windwp/nvim-autopairs",
                config = function()
                        require("nvim-autopairs").setup {}
                end
        }

        -- Terminals
        use 'akinsho/toggleterm.nvim'
end)
