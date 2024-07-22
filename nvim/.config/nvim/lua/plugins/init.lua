return {
  {
    "dstein64/vim-startuptime",
    -- lazy-load on a command
    cmd = "StartupTime",
    -- init is called during startup. Configuration for vim plugins typically should be set in an init function
    init = function()
      vim.g.startuptime_tries = 10
    end,
},
  { "nvim-lua/plenary.nvim" },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  {
    "anuvyklack/hydra.nvim",
    -- config = function()
    -- vim.api.nvim_set_hl(0, "NormalFloat", { fg = "NONE", bg = "NONE" })
    -- require("plugins.keymaps")
    -- end,
  },
  --	{
  -- 	"mrjones2014/legendary.nvim",
  -- 	--   -- since legendary.nvim handles all your keymaps/commands,
  -- 	--   -- its recommended to load legendary.nvim before other plugins
  -- 	priority = 10000,
  -- 	lazy = false,
  -- 	enabled = false,
  -- 	--   -- sqlite is only needed if you want to use frecency sorting
  -- 	dependencies = { "kkharji/sqlite.lua" },
  -- },
  {
    "folke/tokyonight.nvim",
    lazy = false,  -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
      -- load the colorscheme here
      vim.cmd([[colorscheme tokyonight]])
    end,
  },
  {
    "max397574/better-escape.nvim",
    config = function()
      require("better_escape").setup({
        timeout = vim.o.timeoutlen,
        mappings = {
          i = {
            j = {
              -- These can all also be functions
              k = "<Esc>",
              j = "<Esc>",
            },
          },
          c = {
            j = {
              k = "<Esc>",
              j = "<Esc>",
            },
          },
          t = {
            j = {
              k = "<Esc>",
              j = "<Esc>",
            },
          },
          v = {
            j = {
              k = "<Esc>",
            },
          },
          s = {
            j = {
              k = "<Esc>",
            },
          },
        },
      })
    end,
  },
  {
    "folke/neodev.nvim",
    opts = {
      library = { plugins = { "nvim-dap-ui" }, types = true },
    },
  },
  --
  -- { "windwp/nvim-autopairs", event = { "InsertEnter" } }, -- Auto }pairs
  -- NOTE: using autopairs in lsp config
  {
    "altermo/ultimate-autopair.nvim",
    event = { "InsertEnter", "CmdlineEnter" },
    branch = "v0.6",
    -- enabled = false,
    opts = {
      --Config goes here
    },
  },
  {
    "abecodes/tabout.nvim",
    lazy = false,
    config = function()
      require("tabout").setup({
        tabkey = "<Tab>",         -- key to trigger tabout, set to an empty string to disable
        backwards_tabkey = "<S-Tab>", -- key to trigger backwards tabout, set to an empty string to disable
        act_as_tab = true,        -- shift content if tab out is not possible
        act_as_shift_tab = false, -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
        default_tab = "<C-t>",    -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
        default_shift_tab = "<C-d>", -- reverse shift default action,
        enable_backwards = true,  -- well ...
        completion = false,       -- if the tabkey is used in a completion pum
        tabouts = {
          { open = "'", close = "'" },
          { open = '"', close = '"' },
          { open = "`", close = "`" },
          { open = "(", close = ")" },
          { open = "[", close = "]" },
          { open = "{", close = "}" },
        },
        ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
        exclude = {}, -- tabout will ignore these filetypes
      })
    end,
    dependencies = { -- These are optional
      "nvim-treesitter/nvim-treesitter",
      "L3MON4D3/LuaSnip",
      "hrsh7th/nvim-cmp",
    },
    opt = true,            -- Set this to true if the plugin is optional
    event = "InsertCharPre", -- Set the event to 'InsertCharPre' for better compatibility
    priority = 1000,
  },
  --
  --
  {
    "kylechui/nvim-surround",
    version = "v0.6", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end,

    --
    --       Old text                    Command         New text
    -- --------------------------------------------------------------------------------
    --     surr*ound_words             ysiw)           (surround_words)
    --     *make strings               ys$"            "make strings"
    --     [delete ar*ound me!]        ds]             delete around me!
    --     remove <b>HTML t*ags</b>    dst             remove HTML tags
    --     'change quot*es'            cs'"            "change quotes"
    --     <b>or tag* types</b>        csth1<CR>       <h1>or tag types</h1>
    --     delete(functi*on calls)     dsf             function calls
  },
  --
  {
    "numToStr/Comment.nvim",
    opts = function()
      -- add any options here
    end,
    lazy = false,
  },
  -- Search
  -- {
  -- 	"kevinhwang91/nvim-hlslens",
  -- 	config = function()
  -- 		require("hlslens").setup({})
  -- 	end,
  -- },
  --
  -- Search and Replace across multiple files
  {
    "nvim-pack/nvim-spectre",
    event = "VeryLazy",
    keys = {
      { "<leader>S",  '<cmd>lua require("spectre").toggle()<CR>' },
      { "<leader>sw", '<cmd>lua require("spectre").open_visual({select_word=true})<CR>' },
    },
  },


  -- {
  -- 	"kevinhwang91/nvim-ufo",
  -- 	dependencies = {
  -- 		{ "kevinhwang91/promise-async" },
  -- 		{
  -- 			"luukvbaal/statuscol.nvim",
  -- 			config = function()
  -- 				local builtin = require("statuscol.builtin")
  -- 				require("statuscol").setup({
  -- 					relculright = true,
  -- 					segments = {
  -- 						{ text = { builtin.foldfunc }, click = "v:lua.ScFa" },
  -- 						{ text = { "%s" }, click = "v:lua.ScSa" },
  -- 						{ text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
  -- 					},
  -- 				})
  -- 			end,
  -- 		},
  -- 	},
  -- 	event = "VeryLazy",
  -- 	init = function()
  -- 		vim.o.foldcolumn = "1" -- '0' is not bad
  -- 		vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
  -- 		vim.o.foldlevelstart = 99
  -- 		vim.o.foldenable = true
  -- 	end,
  --
  -- 	opts = {
  -- 		preview = {
  -- 			mappings = {
  -- 				scrollB = "<C-b>",
  -- 				scrollF = "<C-f>",
  -- 				scrollU = "<C-u>",
  -- 				scrollD = "<C-d>",
  -- 			},
  -- 		},
  -- 		provider_selector = function(_, filetype, buftype)
  -- 			local function handleFallbackException(bufnr, err, providerName)
  -- 				if type(err) == "string" and err:match("UfoFallbackException") then
  -- 					return require("ufo").getFolds(bufnr, providerName)
  -- 				else
  -- 					return require("promise").reject(err)
  -- 				end
  -- 			end
  --
  -- 			-- only use indent until a file is opened
  -- 			return (filetype == "" or buftype == "nofile") and "indent"
  -- 				or function(bufnr)
  -- 					return require("ufo")
  -- 						.getFolds(bufnr, "lsp")
  -- 						:catch(function(err)
  -- 							return handleFallbackException(bufnr, err, "treesitter")
  -- 						end)
  -- 						:catch(function(err)
  -- 							return handleFallbackException(bufnr, err, "indent")
  -- 						end)
  -- 				end
  -- 		end,
  -- 	},
  --
  -- 	config = function()
  -- 		vim.keymap.set("n", "zR", require("ufo").openAllFolds)
  -- 		vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
  -- 		vim.keymap.set("n", "zr", require("ufo").openFoldsExceptKinds)
  -- 		vim.keymap.set("n", "zm", require("ufo").closeFoldsWith) -- closeAllFolds == closeFoldsWith(0)
  -- 		vim.keymap.set("n", "zp", require("ufo").peekFoldedLinesUnderCursor, { desc = "peek-folds" })
  -- 		vim.api.nvim_set_hl(0, "MoreMsg", { bg = "none", fg = "#7E9CD8" })
  --
  -- 		local handler = function(virtText, lnum, endLnum, width, truncate)
  -- 			local newVirtText = {}
  -- 			local totalLines = vim.api.nvim_buf_line_count(0)
  -- 			local foldedLines = endLnum - lnum
  -- 			local suffix = ("  %d %d%%"):format(foldedLines, foldedLines / totalLines * 100)
  -- 			local sufWidth = vim.fn.strdisplaywidth(suffix)
  -- 			local targetWidth = width - sufWidth
  -- 			local curWidth = 0
  -- 			for _, chunk in ipairs(virtText) do
  -- 				local chunkText = chunk[1]
  -- 				local chunkWidth = vim.fn.strdisplaywidth(chunkText)
  -- 				if targetWidth > curWidth + chunkWidth then
  -- 					table.insert(newVirtText, chunk)
  -- 				else
  -- 					chunkText = truncate(chunkText, targetWidth - curWidth)
  -- 					local hlGroup = chunk[2]
  -- 					table.insert(newVirtText, { chunkText, hlGroup })
  -- 					chunkWidth = vim.fn.strdisplaywidth(chunkText)
  -- 					-- str width returned from truncate() may less than 2nd argument, need padding
  -- 					if curWidth + chunkWidth < targetWidth then
  -- 						suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
  -- 					end
  -- 					break
  -- 				end
  -- 				curWidth = curWidth + chunkWidth
  -- 			end
  -- 			local rAlignAppndx = math.max(math.min(vim.opt.textwidth["_value"], width - 1) - curWidth - sufWidth, 0)
  -- 			suffix = (" "):rep(rAlignAppndx) .. suffix
  -- 			table.insert(newVirtText, { suffix, "MoreMsg" })
  -- 			return newVirtText
  -- 		end
  --
  -- 		require("ufo").setup({
  -- 			provider_selector = function(bufnr, filetype, buftype)
  -- 				return { "treesitter", "indent" }
  -- 			end,
  -- 			fold_virt_text_handler = handler,
  -- 		})
  -- 	end,
  -- },
  {
    "yamatsum/nvim-cursorline",
  },



  {
    "ggandor/flit.nvim",
    event = "VeryLazy",
    dependencies = {
      {
        "ggandor/leap.nvim",
        dependencies = { "tpope/vim-repeat" },
      },
    },
    config = function()
      require("flit").setup({
        keys = { f = "f", F = "F", t = "t", T = "T" },
        -- A string like "nv", "nvo", "o", etc.
        labeled_modes = "v",
        multiline = true,
        -- Like `leap`s similar argument (call-specific overrides).
        -- E.g.: opts = { equivalence_classes = {} }
        opts = {},
      })
    end,
  },
  {
    -- NOTE: fix this
    "mbbill/undotree",
    keys = {
      { "<leader>u", ":UndotreeToggle<CR>", desc = "Toggle Undo Tree" },
    },
  },
  {
    "ethanholz/nvim-lastplace",
    opts = {
      lastplace_ignore_buftype = { "quickfix", "nofile", "help" },
      lastplace_ignore_filetype = {
        "gitcommit",
        "gitrebase",
        "svn",
        "hgcommit",
      },
      lastplace_open_folds = true,
    },
  },
  {
    -- TODO: Integrate nap with Hydra
    "liangxianzhe/nap.nvim",
    lazy = false,
    config = function()
      local Hydra = require("hydra")
      Hydra({
        name = "Side scroll",
        mode = "n",
        body = "<leader>z",
        heads = {
          { "h", "5zh" },
          { "l", "5zl", { desc = "←/→" } },
          { "H", "zH" },
          { "L", "zL", { desc = "half screen ←/→" } },
        },
      })
    end,
  },
  { "echasnovski/mini.cursorword",   version = "*" },
  {
    "Julian/vim-textobj-variable-segment",
    keys = { { "av", mode = { "x", "o" } }, { "iv", mode = { "x", "o" } } },
    dependencies = { "kana/vim-textobj-user" },
  },
  { "adaszko/tree_climber_rust.nvim" },
  -- idention
  {
    "michaeljsmith/vim-indent-object",
    keys = {
      { "ai", mode = { "x", "o" } },
      { "ii", mode = { "x", "o" } },
      { "aI", mode = { "x", "o" } },
      { "iI", mode = { "x", "o" } },
    },
  },
}
