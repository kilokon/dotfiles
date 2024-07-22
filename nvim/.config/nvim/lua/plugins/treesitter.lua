return {
	{
		"nvim-treesitter/nvim-treesitter",
		event = { "BufRead", "BufNewFile", "InsertEnter" },
		-- event = "User BaseFile",
		-- event = { "BufReadPost", "BufNewFile" },
		-- cmd = {
		-- 	"TSBufDisable",
		-- 	"TSBufEnable",
		-- 	"TSBufToggle",
		-- 	"TSDisable",
		-- 	"TSEnable",
		-- 	"TSToggle",
		-- 	"TSInstall",
		-- 	"TSInstallInfo",
		-- 	"TSInstallSync",
		-- 	"TSModuleInfo",
		-- 	"TSUninstall",
		-- 	"TSUpdate",
		-- 	"TSUpdateSync",
		-- },
		dependencies = {
			{ "nvim-treesitter/playground" },

			{ "nvim-treesitter/nvim-treesitter-textobjects", event = "VeryLazy" },
			{ "hiphish/rainbow-delimiters.nvim", event = "VeryLazy" },
			-- { "/nvim-ts-rainbow2", event = "VeryLazy" },

			{ "mimmanuel/nvim-treesitter-powershell" },
			{ "nushell/tree-sitter-nu" },
		},
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")
			local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

			require("nvim-treesitter.parsers").get_parser_configs().just = {
				install_info = {
					url = "https://github.com/IndianBoy42/tree-sitter-just", -- local path or git repo
					files = { "src/parser.c", "src/scanner.cc" },
					branch = "main",
					-- use_makefile = true -- this may be necessary on MacOS (try if you see compiler errors)
				},
				filetype = "justfile",
				maintainers = { "@IndianBoy42" },
			}

			-- IMPORTANT: Install tree-sitter-cli
			parser_config.PowerShell = require("ts-powershell").parser_config

			configs.setup({
				ensure_installed = {
					"bash",
					"c",
					-- "cpp",
					-- "csv",
					-- "cmake",
					-- "dockerfile",
					-- "fennel",
					-- "fish",
					-- "gitignore",
					-- "haskell",
					-- "html",
					-- "just",
					-- "lua",
					-- "make",
					-- "meson",
					-- "nix",
					-- "PowerShell",
					-- "python",
					-- "qmljs",
					-- "query",
					-- "ron",
					-- "rust",
					-- "toml",
					-- "vim",
					-- "vimdoc",
					-- "javascript",
				},
				query_linter = {
					enable = true,
					use_virtual_text = true,
					lint_events = { "BufWrite", "CursorHold" },
				},
				sync_install = false,
				highlight = { enable = true },
				indent = { enable = true },
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "<C-space>",
						node_incremental = "<C-space>",
						scope_incremental = false,
						node_decremental = "<bs>",
					},
				},
				matchup = {
					enable = true, -- mandatory, false will disable the whole extension
					-- disable = { "yaml" },
				},
				-- rainbow = {
				-- 	enable = true,
				-- 	-- list of languages you want to disable the plugin for
				-- 	-- disable = { "jsx", "cpp" },
				-- 	-- Which query to use for finding delimiters
				-- 	query = "rainbow-parens",
				-- 	-- Highlight the entire buffer all at once
				-- 	strategy = require("ts-rainbow").strategy.global,
				-- },
			})
		end,
	},
	{
		"chrisgrieser/nvim-various-textobjs",
		lazy = false,
		opts = { useDefaultKeymaps = true },
	},
	{ "chrisgrieser/nvim-spider", lazy = true },
	-- keymaps in legendary nvim
}
