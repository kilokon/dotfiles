return {
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.8",
		-- or                              , branch = '0.1.x',
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-z.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
			},
			{
				"benfowler/telescope-luasnip.nvim",
				module = "telescope._extensions.luasnip", -- if you wish to lazy-load
			},
			"tsakirist/telescope-lazy.nvim",
			"nvim-telescope/telescope-github.nvim",
			"nvim-telescope/telescope-frecency.nvim",
			"nvim-telescope/telescope-file-browser.nvim",
			"https://codeberg.org/elfahor/telescope-just.nvim",
			{
				"nvim-telescope/telescope-project.nvim",
				dependencies = {
					"ThePrimeagen/harpoon",
				},
			},
			{
				"prochri/telescope-all-recent.nvim",
				dependencies = {
					"kkharji/sqlite.lua",
					-- optional, if using telescope for vim.ui.select
					"stevearc/dressing.nvim",
				},
			},
		},
		keys = {
			{ "<leader>ff", "<cmd>Telescope frecency<cr>", desc = "Telescope Find" },
			{ "<leader>fo", "<cmd> Telescope oldfiles <CR>", desc = "Search Recent" },
			{ "<leader>fk", "<cmd> Telescope keymaps <CR>", desc = "Show Keys" },
			{ "<leader>fg", "<cmd> Telescope git_status <CR>", desc = "Show Git Status" },
			{ "<leader>fs", "<cmd> Telescope treesitter <CR>", desc = "Show Treesitter Symbols" },
			{
				"<leader>fb",
				"<cmd> Telescope file_browser path=%:p:h select_buffer=true<CR>",
				desc = "Open File Browser",
			},
		},
		config = function()
			local actions = require("telescope.actions")
			local project_actions = require("telescope._extensions.project.actions")
			require("telescope").setup({
				defaults = {
					-- Default configuration for telescope goes here:
					-- config_key = value,
					vimgrep_arguments = {
						"rg",
						"--color=never",
						"--no-heading",
						"--with-filename",
						"--line-number",
						"--column",
						"--smart-case",
						"--trim", -- add this value
					},
					mappings = {
						i = {
							-- map actions.which_key to <C-h> (default: <C-/>)
							-- actions.which_key shows the mappings for your picker,
							-- e.g. git_{create, delete, ...}_branch for the git_branches picker
							["<esc>"] = actions.close,
							["<C-h>"] = "which_key",
						},
					},
				},
				pickers = {
					-- Default configuration for builtin pickers goes here:
					-- picker_name = {
					--   picker_config_key = value,
					--   ...
					-- }
					-- Now the picker_config_key will be applied every time you call this
					-- builtin picker
					find_files = {
						find_command = { "fd", "--type", "f", "--strip-cwd-prefix" },
					},
					mappings = {
						n = {
							["cd"] = function(prompt_bufnr)
								local selection = require("telescope.actions.state").get_selected_entry()
								local dir = vim.fn.fnamemodify(selection.path, ":p:h")
								require("telescope.actions").close(prompt_bufnr)
								-- Depending on what you want put `cd`, `lcd`, `tcd`
								vim.cmd(string.format("silent lcd %s", dir))
							end,
						},
					},
				},
				extensions = {
					fzf = {
						fuzzy = true, -- false will only do exact matching
						override_generic_sorter = true, -- override the generic sorter
						override_file_sorter = true, -- override the file sorter
						case_mode = "smart_case", -- or "ignore_case" or "respect_case"
						-- the default case_mode is "smart_case"
					},
					lazy = {
						-- Optional theme (the extension doesn't set a default theme)
						theme = "ivy",
						-- Whether or not to show the icon in the first column
						show_icon = true,
						-- Mappings for the actions
						mappings = {
							open_in_browser = "<C-o>",
							open_in_file_browser = "<M-b>",
							open_in_find_files = "<C-f>",
							open_in_live_grep = "<C-g>",
							open_in_terminal = "<C-t>",
							open_plugins_picker = "<C-b>", -- Works only after having called first another action
							open_lazy_root_find_files = "<C-r>f",
							open_lazy_root_live_grep = "<C-r>g",
							change_cwd_to_plugin = "<C-c>d",
						},
						-- Extra configuration options for the actions
						actions_opts = {
							open_in_browser = {
								-- Close the telescope window after the action is executed
								auto_close = false,
							},
							change_cwd_to_plugin = {
								-- Close the telescope window after the action is executed
								auto_close = false,
							},
						},
						-- Configuration that will be passed to the window that hosts the terminal
						-- For more configuration options check 'nvim_open_win()'
						terminal_opts = {
							relative = "editor",
							style = "minimal",
							border = "rounded",
							title = "Telescope lazy",
							title_pos = "center",
							width = 0.5,
							height = 0.5,
						},
						-- Other telescope configuration options
					},
					just = {
						-- I rather suggest dropdown!
						theme = "ivy",
						-- A good option is to show a popup window.
						-- You can do that with tmux or toggleterm.
						action = function(command)
							vim.fn.system(command)
							print("Executed", command)
						end,
					},
					file_browser = {
						theme = "ivy",
						-- disables netrw and use telescope-file-browser in its place
						hijack_netrw = true,
						mappings = {
							["i"] = {
								-- your custom insert mode mappings
							},
							["n"] = {
								-- your custom normal mode mappings
							},
						},
					},
					project = {
						base_dirs = {
							"~/sync",
							{ "~/Dev" },
							{ "~/OneDrive/dev", max_depth = 4 },
							-- { path = "~/dev/src5", max_depth = 2 },
						},
						hidden_files = true, -- default: false
						theme = "dropdown",
						order_by = "asc",
						search_by = "title",
						sync_with_nvim_tree = true, -- default false
						-- default for on_project_selected = find project files
						on_project_selected = function(prompt_bufnr)
							-- Do anything you want in here. For example:
							project_actions.change_working_directory(prompt_bufnr, false)
							require("harpoon.ui").nav_file(1)
						end,
					},
				},
			})
			require("telescope").load_extension("fzf")
			require("telescope").load_extension("z")
			require("telescope").load_extension("lazy")
			require("telescope").load_extension("luasnip")
			require("telescope").load_extension("frecency")
			require("telescope").load_extension("gh")
			require("telescope").load_extension("project")
			require("telescope").load_extension("just")
			require("telescope").load_extension("file_browser")
		end,
	},

	-- "nvim-telescope/telescope.nvim",
	-- branch = "0.1.x",
	-- dependencies = {
	--   { "nvim-lua/plenary.nvim" },
	--   "benfowler/telescope-luasnip.nvim",
	--   "debugloop/telescope-undo.nvim",
	-- },
	-- cmd = "Telescope",
	-- event = "VeryLazy",
	-- config = function(_, opts)
	--   require("telescope").setup({
	--     pickers = {
	--       find_files = {
	--         file_ignore_patterns = { "venv/", "__pycache__/", ".git/", ".idea/" },
	--       },
	--     },
	--   })
	-- end,
	-- {
	-- 	"nvim-telescope/telescope-file-browser.nvim",
	-- 	dependencies = {
	-- 		"nvim-telescope/telescope.nvim",
	-- 		branch = "0.1.x",
	-- 		dependencies = {
	-- 			{ "nvim-lua/plenary.nvim" },
	-- 			"benfowler/telescope-luasnip.nvim",
	-- 			"debugloop/telescope-undo.nvim",
	-- 		},
	-- 		cmd = "Telescope",
	-- 		event = "VeryLazy",
	-- 		config = function(_, opts)
	-- 			require("telescope").setup({
	-- 				pickers = {
	-- 					find_files = {
	-- 						file_ignore_patterns = { "venv/", "__pycache__/", ".git/", ".idea/" },
	-- 					},
	-- 				},
	-- 			})
	-- 			require("telescope").load_extension("file_browser")
	-- 			require("telescope").load_extension("noice")
	-- 			require("telescope").load_extension("fzf")
	-- 			require("telescope").load_extension("smart_open")
	-- 			require("telescope").load_extension("frecency")
	-- 			require("telescope").load_extension("luasnip")
	-- 			require("telescope").load_extension("undo")
	-- 			--     require("telescope").load_extension("live_grep_args")
	-- 		end,
	--
	-- 		"nvim-lua/plenary.nvim",
	-- 	},
	-- },
}

-- local telescope = {
--   "nvim-telescope/telescope.nvim",
--   dependencies = {
--     -- https://github.com/orgs/nvim-telescope/repositories
--     {
--       "danielfalk/smart-open.nvim",
--       dependencies = { "kkharji/sqlite.lua" },
--     },
--     {
--       "nvim-telescope/telescope-frecency.nvim",
--       dependencies = {
--         "kkharji/sqlite.lua",
--       },
--     },
--     "nvim-telescope/telescope-media-files.nvim",
--     "nvim-telescope/telescope-github.nvim",
--     "nvim-telescope/telescope-live-grep-args.nvim",
--     {
--       "nvim-telescope/telescope-fzf-native.nvim",
--       -- NOTE: If you are having trouble with this installation,
--       --       refer to the README for telescope-fzf-native for more instructions.
--       build = "make",
--       cond = function()
--         return vim.fn.executable("make") == 1
--       end,
--     },
--     "LukasPietzschmann/telescope-tabs",
--     "benfowler/telescope-luasnip.nvim",
--     "debugloop/telescope-undo.nvim",
--   },
--   cmd = "Telescope",
--   opts = function()
--     -- local actions = setmetatable({}, {
--     --   __index = function(t, k)
--     --     return function(...) return require("telescope.actions")[k](...) end
--     --   end,
--     -- })
--     local actions = require("telescope.actions")
--     local lga_actions = require("telescope-live-grep-args.actions")
--     local action_layout = require("telescope.actions.layout")
--     local previewers = require("telescope.previewers")
--
--     local with_rg = require("utils.telescope").with_rg
--     local rg = with_rg({ ignore = true, hidden = true })
--     local fd = with_rg({ ignore = true, hidden = true, files = true })
--
--     return {
--       defaults = {
--         find_command = fd,
--         vimgrep_arguments = rg,
--         prompt_prefix = " ",
--         selection_caret = " ",
--         initial_mode = "insert",
--         layout_strategy = "flex",
--         layout_config = {
--           width = 0.75,
--           prompt_position = "bottom",
--           preview_cutoff = 120,
--           horizontal = { mirror = false },
--           vertical = {
--             mirror = false,
--             preview_cutoff = 2,
--           },
--           flex = {
--             flip_columns = 150,
--           },
--         },
--         path_display = { "smart" },
--         set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
--         mappings = {
--           i = {
--             ["<C-p>"] = action_layout.toggle_preview,
--             ["<Esc>"] = actions.close,
--
--             ["<C-h>"] = utils.telescope.flash,
--             ["<C-x>"] = actions.delete_buffer,
--             ["<C-s>"] = actions.select_horizontal,
--             ["<C-v>"] = actions.select_vertical,
--             ["<C-t>"] = actions.select_tab,
--             ["<C-j>"] = actions.move_selection_next,
--             ["<C-k>"] = actions.move_selection_previous,
--             ["<CR>"] = actions.select_default,
--             ["<C-u>"] = actions.preview_scrolling_up,
--             ["<C-d>"] = actions.preview_scrolling_down,
--             ["<C-up>"] = actions.cycle_history_next,
--             ["<C-down>"] = actions.cycle_history_prev,
--             ["<C-S-q>"] = function(...)
--               actions.send_to_qflist(...)
--               actions.open_qflist(...)
--               require("replacer").run()
--             end,
--             ["<C-q>"] = function(...)
--               actions.send_to_qflist(...)
--               actions.open_qflist(...)
--             end,
--             ["<C-l>"] = function(...)
--               require("trouble.providers.telescope").open_with_trouble(...)
--             end,
--             -- ["<C-y>"] = functions.set_prompt_to_entry_value,
--             ["<C-cr>"] = utils.telescope.select_pick_window,
--             ["<M-cr>"] = utils.telescope.select_pick_window,
--             -- ["<C-Space>"] = to fuzzy
--             ["<tab>"] = function(prompt_bufnr)
--               vim.api.nvim_buf_call(prompt_bufnr, function()
--                 vim.cmd("normal! A.*?")
--               end)
--             end,
--           },
--           n = {
--             -- ["<M-p>"] = action_layout.toggle_preview,
--             ["h"] = utils.telescope.flash,
--             ["j"] = actions.move_selection_next,
--             ["k"] = actions.move_selection_previous,
--             ["<localleader>x"] = actions.delete_buffer,
--             ["<localleader>s"] = actions.select_horizontal,
--             ["<localleader>v"] = actions.select_vertical,
--             ["<localleader>t"] = actions.select_tab,
--             ["<CR>"] = actions.select_default,
--             ["<C-up>"] = actions.preview_scrolling_up,
--             ["<C-down>"] = actions.preview_scrolling_down,
--             ["<localleader>q"] = function(...)
--               actions.send_selected_to_qflist(...)
--               actions.open_qflist(...)
--             end,
--             ["<C-c>"] = actions.close,
--             ["<localleader>n"] = utils.telescope.select_pick_window,
--           },
--         },
--       },
--       pickers = {
--         live_grep = {
--           on_input_filter_cb = function(prompt)
--             -- AND operator for live_grep like how fzf handles spaces with wildcards in rg
--             return { prompt = prompt:gsub("%s", ".*?") }
--           end,
--         },
--         git_branches = {
--           attach_mappings = function(_, map)
--             map("i", "<c-x>", actions.git_delete_branch)
--             map("n", "<c-x>", actions.git_delete_branch)
--             map("i", "<c-y>", M.set_prompt_to_entry_value)
--             return true
--           end,
--         },
--       },
--       extensions = {
--         fzy_native = {
--           override_generic_sorter = false,
--           override_file_sorter = false,
--         },
--         fzf = {
--           fuzzy = true, -- false will only do exact matching
--           override_generic_sorter = true, -- override the generic sorter
--           override_file_sorter = true, -- override the file sorter
--           case_mode = "smart_case", -- or "ignore_case" or "respect_case"
--           -- the default case_mode is "smart_case"
--         },
--         "cmake",
--         advanced_git_search = {
--           -- fugitive or diffview
--           diff_plugin = "diffview",
--           -- customize git in previewer
--           -- e.g. flags such as { "--no-pager" }, or { "-c", "delta.side-by-side=false" }
--           git_flags = {},
--           -- customize git diff in previewer
--           -- e.g. flags such as { "--raw" }
--           git_diff_flags = {},
--         },
--         undo = {},
--         smart_open = {
--           match_algorithm = "fzf",
--         },
--         live_grep_args = {
--           auto_quoting = true,
--           mappings = {
--             i = {
--               ["<C-k>"] = lga_actions.quote_prompt(),
--               ["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
--               ["<C-S-i>"] = lga_actions.quote_prompt({ postfix = " --t " }),
--             },
--           },
--         },
--       },
--     }
--   end,
--   config = function(_, opts)
--     local telescope = require("telescope")
--     telescope.setup(opts)
--
--     -- telescope.load_extension('fzy_native')
--     telescope.load_extension("noice")
--     telescope.load_extension("fzf")
--     telescope.load_extension("smart_open")
--     telescope.load_extension("frecency")
--     telescope.load_extension("luasnip")
--     telescope.load_extension("undo")
--     telescope.load_extension("live_grep_args")
--     -- telescope.load_extension('project')
--   end,
-- }
-- local M = {
--   telescope,
-- }
-- return M
