return {
	{
		"VonHeikemen/lsp-zero.nvim",
		branch = "dev-v3",
		lazy = true,
		config = false,
		init = function()
			-- Disable automatic setup, we are doing it manually
			-- vim.g.lsp_zero_extend_cmp = 0
			-- vim.g.lsp_zero_extend_lspconfig = 0
			vim.b.lsp_zero_enable_autoformat = 1
		end,
	},

	{
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		dependencies = {
			"rafamadriz/friendly-snippets",
			"zeioth/NormalSnippets",
			"benfowler/telescope-luasnip.nvim",
		},
		-- event = { "InsertEnter" },
		--     opts = {
		--   history = true,
		--   delete_check_events = "TextChanged",
		--   region_check_events = "CursorMoved",
		-- },
		event = "User BaseFile",
		config = function()
			local ls = require("luasnip")
			local types = require("luasnip.util.types")

			ls.config.set_config({
				history = true,
				-- Snippets aren't automatically removed if their text is deleted.
				-- `delete_check_events` determines on which events (:h events) a check for
				-- deleted snippets is performed.
				-- This can be especially useful when `history` is enabled.
				delete_check_events = "TextChanged",
				ext_opts = {
					[types.choiceNode] = {
						active = {
							virt_text = { { "choiceNode", "Comment" } },
						},
					},
				},
				-- treesitter-hl has 100, use something higher (default is 200).
				ext_base_prio = 300,
				-- minimal increase in priority.
				ext_prio_increase = 1,
				enable_autosnippets = true,
				-- mapping for cutting selected text so it's usable as SELECT_DEDENT,
				-- SELECT_RAW or TM_SELECTED_TEXT (mapped via xmap).
				store_selection_keys = "<c-k>",
			})
			vim.tbl_map(function(type)
				require("luasnip.loaders.from_" .. type).lazy_load()
			end, { "vscode", "snipmate", "lua" })

			require("luasnip.loaders.from_lua").lazy_load({ paths = { vim.fn.stdpath("config") .. "/snippets" } })

			-- <c-l> is selecting within a list of options.
			vim.keymap.set({ "s", "i" }, "<c-l>", function()
				if ls.choice_active() then
					ls.change_choice(1)
				end
			end, { desc = "Scroll through choice nodes" })

			-- <c-k> is my expansion key
			-- this will expand the current item or jump to the next item within the snippet.
			vim.keymap.set({ "i", "s" }, "<c-k>", function()
				if ls.expand_or_jumpable() then
					ls.expand_or_jump()
				end
			end, { silent = true })

			-- <c-j> is my jump backwards key.
			-- this always moves to the previous item within the snippet
			vim.keymap.set({ "i", "s" }, "<c-j>", function()
				if ls.jumpable(-1) then
					ls.jump(-1)
				end
			end, { silent = true })
		end,
	},

	-- Autocompletion
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		dependencies = {
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-path" }, -- vim/neovim }snippet stuffs
			{ "hrsh7th/cmp-cmdline" }, -- vim/n}eovim snippet stuffs
			{ "saadparwaiz1/cmp_luasnip" },
			{ "hrsh7th/cmp-nvim-lsp-signature-help" },
		},
		config = function()
			-- Here is where you configure the autocompletion settings.
			local lsp_zero = require("lsp-zero")
			lsp_zero.extend_cmp()

			-- And you can configure cmp even more, if you want to.
			local cmp = require("cmp")
			-- local cmp_format = require("lsp-zero").cmp_format()
			local cmp_action = require("lsp-zero").cmp_action()
			local luasnip = require("luasnip")
			require("luasnip.loaders.from_vscode").lazy_load()

			cmp.setup({
				-- formatting = lsp_zero.cmp_format(),
				fields = { "abbr", "kind", "menu" },
				-- formatting = cmp_format,
				formatting = {
					fields = { "abbr", "kind", "menu" },
					format = require("lspkind").cmp_format({
						mode = "symbol", -- show only symbol annotations
						maxwidth = 50, -- prevent the popup from showing more than provided characters
						ellipsis_char = "...", -- when popup menu exceed maxwidth,
					}),
				},
				preselect = "none",
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				sources = {
					{ name = "luasnip" },
					{ name = "nvim_lsp" },
					{ name = "nvim_lua" },
					{ name = "path" },
					-- { name = "crates" },
					-- { name = "conjure" },
				},
				completion = {
					keyword_length = 2,
					-- autocomplete = true
					completeopt = "menu,menuone,noinsert",
				},

				mapping = cmp.mapping.preset.insert({
					["<C-Space>"] = cmp.mapping.complete(),
					["<C-u>"] = cmp.mapping.scroll_docs(-4),
					["<C-d>"] = cmp.mapping.scroll_docs(4),
					["<C-f>"] = cmp_action.luasnip_jump_forward(),
					["<C-b>"] = cmp_action.luasnip_jump_backward(),
					["<Down>"] = cmp.mapping(function(fallback)
						cmp.close()
						fallback()
					end, { "i" }),
					["<Up>"] = cmp.mapping(function(fallback)
						cmp.close()
						fallback()
					end, { "i" }),
					["<Tab>"] = cmp_action.luasnip_supertab(),
					["<S-Tab>"] = cmp_action.luasnip_shift_supertab(),
					-- ["<Tab>"] = cmp_action.tab_complete(),
					-- ["<S-Tab>"] = cmp_action.select_prev_or_fallback(),
					["<CR>"] = cmp.mapping.confirm({
						behavior = cmp.ConfirmBehavior.Replace,
						select = false,
					}),
					["<ScrollWheelUp>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), {
						"i",
						"c",
					}),
					["<ScrollWheelDown>"] = cmp.mapping(cmp.mapping.scroll_docs(4), {
						"i",
						"c",
					}),
				}),
			})
			cmp.setup.cmdline("/", {
				sources = {
					{ name = "buffer" },
					{ name = "treesitter" },
				},
			})

			cmp.setup.cmdline(":", {
				sources = cmp.config.sources({
					{ name = "path" },
				}, {
					{ name = "cmdline" },
				}),
			})
			cmp.setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
				sources = { name = "dap" },
			})
		end,
	},

	{
		"neovim/nvim-lspconfig",
		cmd = { "LspInfo", "LspInstall", "LspStart" },
		-- event = { "BufReadPre" },
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {

			{
				"williamboman/mason.nvim",
				cmd = { "Mason", "MasonInstall", "MasonUpdate" },
				lazy = true,
				opts = {
					ensure_installed = {},
					ui = {
						border = "rounded",
					},
				},
				config = true,
			},
			{ "williamboman/mason-lspconfig.nvim", enabled = true },
			-- { "simrat39/inlay-hints.nvim" },
			{ "lvimuser/lsp-inlayhints.nvim" },
			-- {
			-- 	"mfussenegger/nvim-lint",
			-- 	event = { "BufReadPre", "BufNewFile" },
			-- 	config = function()
			-- 		local lint = require("lint")
			-- 		lint.linters_by_ft = {
			-- 			markdown = { "markdownlint" },
			-- 			cmake = { "cmakelint" },
			-- 			cpp = { "clang-tidy" },
			-- 		}
			-- 	end,
			-- }, -- {
			-- {
			-- 	"rshkarin/mason-nvim-lint",
			-- 	--   dependencies = {
			-- 	--   "mfussenegger/nvim-lint",
			-- 	--   },
			-- 	opts = {
			-- 		ensure_installed = {
			-- 			"clang-tidy",
			-- 			"cmakelint",
			-- 			"eslint_d",
			-- 			"jsonlint",
			-- 			"Ruff",
			-- 		},
			-- 	},
			-- },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "lukas-reineke/lsp-format.nvim" },
			{ "onsails/lspkind.nvim" },
			{ "b0o/schemastore.nvim" },
			{ "simrat39/rust-tools.nvim" },
			{
				"dnlhc/glance.nvim",
				keys = {
					{ "gD", "<CMD>Glance definitions<CR>" },
					{ "gR", "<CMD>Glance references<CR>" },
					{ "gY", "<CMD>Glance type_definitions<CR>" },
					{ "gM", "<CMD>Glance implementations<CR>" },
				},
			},
			{
				"SmiteshP/nvim-navic",
				config = function()
					require("nvim-navic").setup({
						icons = {
							File = " ",
							Module = " ",
							Namespace = " ",
							Package = " ",
							Class = " ",
							Method = " ",
							Property = " ",
							Field = " ",
							Constructor = " ",
							Enum = "練",
							Interface = "練",
							Function = " ",
							Variable = " ",
							Constant = " ",
							String = " ",
							Number = " ",
							Boolean = "◩ ",
							Array = " ",
							Object = " ",
							Key = " ",
							Null = "ﳠ ",
							EnumMember = " ",
							Struct = " ",
							Event = " ",
							Operator = " ",
							TypeParameter = " ",
						},
						highlight = true,
					})
				end,
			},
		},

		config = function()
			-- This is where all the LSP shenanigans will live

			local lsp_zero = require("lsp-zero")
			lsp_zero.extend_lspconfig()
			-- local ih = require("inlay-hints")
			local ih = require("lsp-inlayhints")
			ih.setup()

			lsp_zero.on_attach(function(client, bufnr)
				-- see :help lsp-zero-keybindings
				-- to learn the available actions
				lsp_zero.default_keymaps({
					buffer = bufnr,
					exclude = { "gD", "gR", "gY", "gM" }, -- Glance will handle these
				})

				if client.server_capabilities.documentSymbolProvider then
					require("nvim-navic").attach(client, bufnr)
				end
			end)

			vim.g.haskell_tools = {
				hls = {
					capabilities = lsp_zero.get_capabilities(),
				},
			}

			-- Autocmd that will actually be in charging of starting hls
			local hls_augroup = vim.api.nvim_create_augroup("haskell-lsp", { clear = true })
			vim.api.nvim_create_autocmd("FileType", {
				group = hls_augroup,
				pattern = { "haskell" },
				callback = function()
					---
					-- Suggested keymaps from the quick setup section:
					-- https://github.com/mrcjkb/haskell-tools.nvim#quick-setup
					---
					local ht = require("haskell-tools")
					local bufnr = vim.api.nvim_get_current_buf()
					local def_opts = { noremap = true, silent = true, buffer = bufnr }
					local opts = { noremap = true, silent = true }

					-- haskell-language-server relies heavily on codeLenses,
					-- so auto-refresh (see advanced configuration) is enabled by default
					vim.keymap.set("n", "<space>ca", vim.lsp.codelens.run, opts)
					-- Hoogle search for the type signature of the definition under the cursor
					vim.keymap.set("n", "<space>hs", ht.hoogle.hoogle_signature, opts)
					-- Evaluate all code snippets
					vim.keymap.set("n", "<space>ea", ht.lsp.buf_eval_all, opts)
					-- Toggle a GHCi repl for the current package
					vim.keymap.set("n", "<leader>rr", ht.repl.toggle, opts)
					-- Toggle a GHCi repl for the current buffer
					vim.keymap.set("n", "<leader>rf", function()
						ht.repl.toggle(vim.api.nvim_buf_get_name(0))
					end, def_opts)
					vim.keymap.set("n", "<leader>rq", ht.repl.quit, opts)
				end,
			})

			lsp_zero.set_sign_icons({
				error = "✘",
				warn = "▲",
				hint = "⚑",
				info = "»",
			})

			require("mason-lspconfig").setup({
				ensure_installed = {
					-- "ccls",
					"clangd",
					"fennel_language_server",
					-- "hls",
					"lua_ls",
					"cmake",
					"neocmake",
					"powershell_es",
					"pylsp",
					"ruff_lsp",
					"rust_analyzer",
					"taplo",
					"typst_lsp",
					"yamlls",
				},
				handlers = {
					lsp_zero.default_setup,

					--LUA LSP
					lua_ls = function()
						local lua_opts = lsp_zero.nvim_lua_ls()
						require("lspconfig").lua_ls.setup({
							lua_opts,
							root_dir = function()
								--- either .luarc.json or .stylua.toml
								return lsp_zero.dir.find_first({ ".luarc.json", ".stylua.toml" })
							end,
							on_attach = function(client, bufnr)
								-- on_attach,
								ih.on_attach(client, bufnr)
							end,
							settings = {
								Lua = {
									completion = {
										callSnippet = "Replace",
									},
									hint = {
										enable = true,
									},
									diagnostics = {
										globals = { "vim" },
									},
									disable = {
										"luadoc-miss-see-name",
										"undefined-field",
									},
									runtime = {
										version = "LuaJIT",
									},
									workspace = {
										library = {
											vim.env.VIMRUNTIME .. "/lua",
										},
										checkThirdParty = "Disable",
									},
								},
							},
						})
					end,
					-- RUST TOOLS
					rust_analyzer = function()
						local rust_tools = require("rust-tools")

						rust_tools.setup({
							server = {
								on_attach = function(client, bufnr)
									-- on_attach(client, bufnr)
									ih.on_attach(client, bufnr)
									vim.keymap.set(
										"n",
										"<leader>ca",
										rust_tools.hover_actions.hover_actions,
										{ buffer = bufnr }
									)
								end,
							},
							tools = {
								executor = require("rust-tools.executors").toggleterm,
								on_initialized = function()
									-- ih.on_attach(client, bufnr)
									-- ih.set_all()
								end,
								inlay_hints = {
									auto = false,
								},
							},
						})
					end,

					-- JSONLS
					jsonls = function()
						require("lspconfig").jsonls.setup({
							settings = {
								json = {
									schemas = require("schemastore").json.schemas(),
									validate = { enable = true },
								},
							},
						})
					end,
					-- Python Ruff Linter and Formatter
					ruff_lsp = function()
						require("lspconfig").ruff_lsp.setup({
							init_options = {
								settings = {
									-- Any extra CLI arguments for `ruff` go here.
									args = {},
								},
							},
						})
					end,
					pylsp = function()
						require("lspconfig").pylsp.setup({
							settings = {
								pylsp = {
									plugins = {
										ruff = {
											enabled = true,
											extendSelect = { "I" },
										},
									},
								},
							},
						})
					end,
					-- C/C++ LSP
					clangd = function()
						require("lspconfig").clangd.setup({
							cmd = {
								"clangd",
								"--background-index",
								"--clang-tidy",
								"--completion-style=bundled",
								"--header-insertion=iwyu",
							},
							filetypes = { "c", "cpp", "objc", "objcpp", "h", "hpp" },
							on_attach = function(_, _)
								require("clangd_extensions.inlay_hints").setup_autocmd()
								require("clangd_extensions.inlay_hints").set_inlay_hints()
							end,
						})
					end,

					-- CMake File LSP
					-- neocmake = function()
					-- 	require("lspconfig").neocmake.setup({
					-- 		cmd = { "neocmakelsp", "--stdio" },
					-- 		filetypes = { "cmake" },
					-- 		init_options = {
					-- 			format = {
					-- 				enable = true,
					-- 			},
					-- 			scan_cmake_in_package = true, -- default is true
					-- 		},
					-- 		root_dir = function()
					-- 			return lsp_zero.dir.find_first({ ".git", "cmake" })
					-- 		end,
					-- 		single_file_support = true, -- suggested
					-- 		semantic_token = false,
					-- 	})
					-- end,

					-- Typst
					typst_lsp = function()
						require("lspconfig").typst_lsp.setup({
							settings = {
								exportPdf = "onType", -- Choose onType, onSave or never.
								-- serverPath = "" -- Normally, there is no need to uncomment it.
							},
						})
					end,
				},
			})
			-- lsp_zero.skip_server_setup({ "rust_analyzer", "hls" })
			lsp_zero.setup()
		end,
	},
	{
		"mfussenegger/nvim-lint",
		event = { "BufReadPre", "BufNewFile" },
		config = function()
			local lint = require("lint")
			lint.linters_by_ft = {
				markdown = { "markdownlint" },
				cmake = { "cmakelint" },
				-- cpp = { "clang-tidy" },
			}
		end,
	},
	--    {
	-- 	"mfussenegger/nvim-lint",
	-- 	event = { "BufReadPre", "BufNewFile" },
	-- 	dependencies = { "rshkarin/mason-nvim-lint", dependencies = "williamboman/mason.nvim" },
	-- 	config = function()
	-- 		local lint = require("lint")
	--
	-- 		-- lint.linters_by_ft = {
	-- 		-- 	c = { "trivy" },
	-- 		-- 	cpp = { "trivy" },
	-- 		-- 	rust = { "trivy" },
	-- 		-- 	python = { "trivy" },
	-- 		-- 	java = { "trivy" },
	-- 		-- 	javascript = { "trivy" },
	-- 		-- 	typescript = { "trivy" },
	-- 		-- }
	--
	-- 		-- Install linters with mason
	-- 		require("mason-nvim-lint").setup()
	--
	-- 		-- Lint on entering buffer, saving and leaging insert mode
	-- 		local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })
	-- 		vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave", "TextChanged" }, {
	-- 			group = lint_augroup,
	-- 			callback = function()
	-- 				lint.try_lint()
	-- 			end,
	-- 		})
	--
	-- 		vim.keymap.set("n", "<leader>io", function()
	-- 			lint.try_lint()
	-- 		end, { desc = "Lint buffer" })
	-- 	end,
	-- },
	-- {
	--   "rshkarin/mason-nvim-lint",
	--   dependencies = {
	--   "mfussenegger/nvim-lint",
	--   },
	--   opts = {
	--     ensure_installed = {
	--       "clang-tidy",
	--       "cmakelint",
	--       "eslint_d",
	--       "jsonlint",
	--       "Ruff",
	--     }
	--   }
	-- },

	-- DAPS
	{
		"mfussenegger/nvim-dap",
		enabled = vim.fn.has("win32") == 0,
		dependencies = {
			{ "theHamsta/nvim-dap-virtual-text", config = true },
			{ "jbyuki/one-small-step-for-vimkind" },
			{ "nvim-neotest/nvim-nio" },
			{
				"jay-babu/mason-nvim-dap.nvim",
				opts = {
					ensure_installed = { "python", "cppdbg", "chrome", "codelldb" },
					handlers = {
						python = function(config)
							config.adapters = {
								type = "executable",
								command = "/usr/bin/python3",
								args = {
									"-m",
									"debugpy.adapter",
								},
							}
							require("mason-nvim-dap").default_setup(config) -- don't forget this!
						end,
					},
				},
			},

			{
				"rcarriga/nvim-dap-ui",
				opts = { floating = { border = "rounded" } },
				config = function(_, opts)
					local dap, dapui = require("dap"), require("dapui")
					dap.listeners.after.event_initialized["dapui_config"] = function()
						dapui.open()
					end
					dap.listeners.before.event_terminated["dapui_config"] = function()
						dapui.close()
					end
					dap.listeners.before.event_exited["dapui_config"] = function()
						dapui.close()
					end
					dapui.setup(opts)
				end,
			},
			{
				"rcarriga/cmp-dap",
				config = function()
					require("cmp").setup({
						enabled = function()
							return vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt"
								or require("cmp_dap").is_dap_buffer()
						end,
					})

					require("cmp").setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
						sources = {
							{ name = "dap" },
						},
					})
				end,
			},
		},
		config = function()
			local sign = vim.fn.sign_define

			sign("dapbreakpoint", { text = "●", texthl = "dapbreakpoint", linehl = "", numhl = "" })
			sign("dapbreakpointcondition", { text = "●", texthl = "dapbreakpointcondition", linehl = "", numhl = "" })
			sign("daplogpoint", { text = "◆", texthl = "daplogpoint", linehl = "", numhl = "" })
			sign("dapstopped", { text = "", texthl = "diagnosticwarn" })
			sign("dapbreakpoint", { text = "", texthl = "diagnosticinfo" })
			sign("dapbreakpointrejected", { text = "", texthl = "diagnosticerror" })
			-- sign("dapbreakpointcondition", { text = "", texthl = "diagnosticinfo" })
			-- sign("daplogpoint", { text = ".>", texthl = "diagnosticinfo" })
		end,
	},
	--
	-- {
	-- 	"jay-babu/mason-nvim-dap.nvim",
	-- 	dependencies = { "mfussenegger/nvim-dap" },
	-- 	opts = {
	-- 		ensure_installed = { "python", "cppdbg", "chrome", "codelldb" },
	-- 		handlers = {
	-- 			python = function(config)
	-- 				config.adapters = {
	-- 					type = "executable",
	-- 					command = "/usr/bin/python3",
	-- 					args = {
	-- 						"-m",
	-- 						"debugpy.adapter",
	-- 					},
	-- 				}
	-- 				require("mason-nvim-dap").default_setup(config) -- don't forget this!
	-- 			end,
	-- 		},
	-- 	},
	-- },
	-- {
	-- 	"mfussenegger/nvim-dap",
	-- 	dependencies = {
	-- 		{ "theHamsta/nvim-dap-virtual-text", config = true },
	-- 		{ "jbyuki/one-small-step-for-vimkind" },
	-- 	},
	-- 	config = function(_, _)
	-- 		vim.fn.sign_define("dapstopped", { text = "", texthl = "diagnosticwarn" })
	-- 		vim.fn.sign_define("dapbreakpoint", { text = "", texthl = "diagnosticinfo" })
	-- 		vim.fn.sign_define("dapbreakpointrejected", { text = "", texthl = "diagnosticerror" })
	-- 		vim.fn.sign_define("dapbreakpointcondition", { text = "", texthl = "diagnosticinfo" })
	-- 		vim.fn.sign_define("daplogpoint", { text = ".>", texthl = "diagnosticinfo" })
	--
	-- 		-- catppuccin
	-- 		local sign = vim.fn.sign_define
	--
	-- 		sign("dapbreakpoint", { text = "●", texthl = "dapbreakpoint", linehl = "", numhl = "" })
	-- 		sign("dapbreakpointcondition", { text = "●", texthl = "dapbreakpointcondition", linehl = "", numhl = "" })
	-- 		sign("daplogpoint", { text = "◆", texthl = "daplogpoint", linehl = "", numhl = "" })
	-- 	end,
	-- },
	-- {
	-- 	"rcarriga/nvim-dap-ui",
	-- 	opts = { floating = { border = "rounded" } },
	-- 	config = function(_, opts)
	-- 		local dap, dapui = require("dap"), require("dapui")
	-- 		dap.listeners.after.event_initialized["dapui_config"] = function()
	-- 			dapui.open()
	-- 		end
	-- 		dap.listeners.before.event_terminated["dapui_config"] = function()
	-- 			dapui.close()
	-- 		end
	-- 		dap.listeners.before.event_exited["dapui_config"] = function()
	-- 			dapui.close()
	-- 		end
	-- 		dapui.setup(opts)
	-- 	end,
	-- },

	--
	{
		"andythigpen/nvim-coverage",
		cmd = {
			"Coverage",
			"CoverageLoad",
			"CoverageLoadLcov",
			"CoverageShow",
			"CoverageHide",
			"CoverageToggle",
			"CoverageClear",
			"CoverageSummary",
		},
		config = function()
			require("coverage").setup()
		end,
		requires = { "nvim-lua/plenary.nvim" },
	},

	{
		"linux-cultist/venv-selector.nvim",
		dependencies = {
			"neovim/nvim-lspconfig",
			"mfussenegger/nvim-dap",
			"mfussenegger/nvim-dap-python", --optional
			{ "nvim-telescope/telescope.nvim", branch = "0.1.x", dependencies = { "nvim-lua/plenary.nvim" } },
		},
		lazy = false,
		branch = "regexp", -- This is the regexp branch, use this for the new version
		config = function()
			require("venv-selector").setup()
		end,
		keys = {
			{ ",v", "<cmd>VenvSelect<cr>" },
		},
	},
	{
		"zbirenbaum/copilot.lua",
		cmd = "Copilot",
		event = "InsertEnter",
		config = function()
			require("copilot").setup({})
		end,
	},

	{
		"Civitasv/cmake-tools.nvim",
		ft = { "cmake" },
		dependencies = {
			"stevearc/overseer.nvim",
		},
		config = function()
			local osys = require("cmake-tools.osys")
			require("cmake-tools").setup({
				cmake_build_directory = function()
					if osys.iswin32 then
						return "out\\${variant:buildType}"
					end
					return "out/${variant:buildType}"
				end,
				-- cmake_build_directory = "build",
			})
		end,
	},
	{
		"Mythos-404/xmake.nvim",
		branch = "v1",
		lazy = true,
		event = "BufReadPost xmake.lua",
		config = true,
		dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
	},
	{
		"mrcjkb/haskell-tools.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		branch = "2.x.x", -- Recommended
		ft = { "haskell", "lhaskell", "cabal", "cabalproject" },
	},

	-- Linting and Formatting
	{
		"nvimdev/guard.nvim",
		dependencies = {
			"nvimdev/guard-collection",
		},
		-- enabled = false,
		config = function()
			local ft = require("guard.filetype")

			-- ft("c"):fmt("clang-format"):lint("clang-tidy")
			-- ft("cpp"):fmt("clang-format"):lint("clang-tidy")
			ft("c,cpp"):fmt({
				cmd = "clang-format",
				stdin = true,
				ignore_patterns = { "neovim", "vim" },
			})
			ft("python"):fmt("black"):lint("ruff")
			ft("lua"):fmt({
				cmd = "stylua",
				args = { "-" },
				stdin = true,
				ignore_patterns = "%w_spec%.lua",
			})
			ft("rust"):fmt("rustfmt")
			ft("html, css, scss, less, md, yaml, json, xml, typescript, javascript, typescriptreact"):fmt("prettier")

			-- Call setup() LAST!
			require("guard").setup({
				-- the only options for the setup function
				fmt_on_save = true,
				-- Use lsp if no formatter was defined for this filetype
				lsp_as_default_formatter = false,
			})
		end,
	},
	--
	{
		"p00f/clangd_extensions.nvim",
		event = "VeryLazy",
		ft = { "c", "cpp", "h", "hpp" },
		config = function()
			require("clangd_extensions").setup({
				-- Setup
			})
		end,
	},

	{ "NoahTheDuke/vim-just" },
	{ "rafcamlet/nvim-luapad" },
	{ -- This plugin
		"Zeioth/compiler.nvim",
		cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
		dependencies = { "stevearc/overseer.nvim", "nvim-telescope/telescope.nvim" },
		opts = {},
		keys = {
			{ "<leader>cc", "<cmd>CompilerOpen<CR>" },
			{ "<leader>cs", "<cmd>CompilerStop<CR>" },
			{ "<leader>ct", "<cmd>CompilerToggleResults<CR>" },
			{ "<leader>cr", "<cmd>CompilerStop<CR><cmd>CompilerRedo<CR>" },
		},
	},
	{ -- The task runner we use
		"stevearc/overseer.nvim",
		commit = "6271cab7ccc4ca840faa93f54440ffae3a3918bd",
		cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
		lazy = false,
		opts = {
			strategy = {
				"toggleterm",
			},
			task_list = {
				direction = nil,
				-- direction = "bottom",
				min_height = 25,
				max_height = 25,
				default_detail = 1,
				bindings = {
					["q"] = function()
						vim.cmd("OverseerClose")
					end,
				},
			},
		},
		config = function()
			require("overseer").setup({
				templates = { "builtin", "user.run_script", "user.cpp_build" },
			})
		end,
	},
	-- { "krady21/compiler-explorer.nvim" },
	{
		"j-hui/fidget.nvim",
		tag = "legacy",
		event = "LspAttach",
		config = function()
			require("fidget").setup()
		end,
	},

	{
		"saecki/crates.nvim",
		event = { "BufRead Cargo.toml" },
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			require("crates").setup()

			local cmp = require("cmp")
			local config = cmp.get_config()
			table.insert(config.sources, {
				name = "buffer",
				option = {
					sources = {
						{ name = "conjure" },
					},
				},
			})
			cmp.setup(config)
		end,
	},
	{
		"zeioth/garbage-day.nvim",
		dependencies = "neovim/nvim-lspconfig",
		event = "VeryLazy",
		opts = {
			-- your options here
		},
	},
	{ "VidocqH/lsp-lens.nvim" },
	{
		"soulis-1256/eagle.nvim",
		config = function()
			require("eagle").setup({
				-- override the default values found in config.lua
			})
		end,
	},
	-- {
	-- 	"piersolenski/wtf.nvim",
	-- 	dependencies = {
	-- 		"MunifTanjim/nui.nvim",
	-- 	},
	-- 	opts = {},
	-- 	keys = {
	-- 		{
	-- 			"gw",
	-- 			mode = { "n", "x" },
	-- 			function()
	-- 				require("wtf").ai()
	-- 			end,
	-- 			desc = "Debug diagnostic with AI",
	-- 		},
	-- 		{
	-- 			mode = { "n" },
	-- 			"gW",
	-- 			function()
	-- 				require("wtf").search()
	-- 			end,
	-- 			desc = "Search diagnostic with Google",
	-- 		},
	-- 	},
	-- },
}
