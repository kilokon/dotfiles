local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Install plugins
return packer.startup(function(use)
	-- Packer can manage itself
	use("wbthomason/packer.nvim")

	-- Load plugins
	-- VIM enhancements
	--use "ciaranm/securemodelines"
	use("editorconfig/editorconfig-vim")
	use("justinmk/vim-sneak")
	use("nvim-lua/popup.nvim") -- An implementation of the Popup API from vim in Neovim
	use("nvim-lua/plenary.nvim") -- Useful lua functions used ny lots of plugins
	use("kyazdani42/nvim-web-devicons")
	-- GUI enhancements
	--use "itchyny/lightline.vim"
	--  requires = { 'kyazdani42/nvim-web-devicons', opt = true }

	--Color Schemes
	use("folke/tokyonight.nvim")

	-- Fuzzy finder
	use("ibhagwan/fzf-lua")
	use("airblade/vim-rooter")

	-- LSP
        use {'neoclide/coc.nvim', branch = 'release'}	
	use "neovim/nvim-lspconfig" -- enable LSP
	use "williamboman/nvim-lsp-installer" -- simple to use language server installer
	use "nvim-lua/lsp_extensions.nvim"
	-- Semantic language support

	-- cmp plugins
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-buffer") -- buffer completions
	use("hrsh7th/cmp-path") -- path completions
	use("hrsh7th/cmp-cmdline") -- cmdline completions
	use("hrsh7th/cmp-nvim-lsp")
	--use("neovim/nvim-lspconfig") -- enable LSP
	--use("nvim-lua/lsp_extensions.nvim")
	use("ray-x/lsp_signature.nvim")
	--use("williamboman/nvim-lsp-installer")
	use("glepnir/lspsaga.nvim")
	use("nvim-telescope/telescope-ui-select.nvim")
        use("nvim-telescope/telescope.nvim")
	use("runiq/fidget.nvim") --UI for nvim-lsp progress

	-- snippets
	use("hrsh7th/vim-vsnip")
	use("L3MON4D3/LuaSnip") --snippet engine
	use("rafamadriz/friendly-snippets") -- a bunch of snippets to use
	use({ "michaelb/sniprun", run = "bash ./install.sh" })

	-- Debugging
	use("mfussenegger/nvim-dap")
	use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } })

	-- Treesitter
	use({
	        "nvim-treesitter/nvim-treesitter",
	 	run = ":TSUpdate",
	})
	use("JoosepAlviste/nvim-ts-context-commentstring")

	-- Formatter
	use("mhartington/formatter.nvim")

	-- rust
	use("simrat39/rust-tools.nvim")
	use({
	 	"saecki/crates.nvim",
	 	tag = "v0.2.1",
	 	requires = { "nvim-lua/plenary.nvim" },
	 	config = function()
	 		require("crates").setup()
	 	end,
	 })

	--Lua
--	use("folke/lua-dev.nvim")

	-- Syntactic language support
--	use("cespare/vim-toml")
--	use("stephpy/vim-yaml")
	--use "rust-lang/rust.vim"
--	use("rhysd/vim-clang-format")
--	--"use "fatih/vim-go"
--	use("dag/vim-fish")
--	use("godlygeek/tabular")
	use("windwp/nvim-spectre")
        use("plasticboy/vim-markdown")

	-- Ease of Use
	use({
		"akinsho/toggleterm.nvim",
		tag = "v1.*",
	})
	use("machakann/vim-highlightedyank")
	use("andymass/vim-matchup")
	use("folke/which-key.nvim")
	use("nvim-lualine/lualine.nvim")
	use("windwp/nvim-autopairs") -- Autopairs, integrates with both cmp and treesitter
	use("numToStr/Comment.nvim") -- Easily comment stuff
	use({
		"kyazdani42/nvim-tree.lua",
		requires = {
			"kyazdani42/nvim-web-devicons", -- optional, for file icon
		},
	})
	use({
		"phaazon/hop.nvim",
		branch = "v1", -- optional but strongly recommended
		config = function()
			require("hop").setup({ keys = "etovxqpdygfblzhckisuran" })
		end,
	})

	-- Git
	use("lewis6991/gitsigns.nvim")

	-- VIM Dev Packages
	use("RishabhRD/popfix")
	use("hood/popui.nvim")
end)
