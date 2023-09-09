return {
  { "nvim-lua/plenary.nvim" },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  {
    "anuvyklack/hydra.nvim",
    -- config = function()
    -- vim.api.nvim_set_hl(0, "NormalFloat", { fg = "NONE", bg = "NONE" })
    -- require("plugins.keymaps")
    -- end,
  },
  {
    "mrjones2014/legendary.nvim",
    -- since legendary.nvim handles all your keymaps/commands,
    -- its recommended to load legendary.nvim before other plugins
    priority = 10000,
    lazy = false,
    -- sqlite is only needed if you want to use frecency sorting
    -- dependencies = { 'kkharji/sqlite.lua' }
  },
  -- {
  --   "nacro90/numb.nvim",
  --   event = "CmdLineEnter",
  --   opts = {
  --     show_numbers = true, -- Enable 'number' for the window while peeking
  --     show_cursorline = true, -- Enable 'cursorline' for the window while peeking
  --   },
  -- },
  -- {
  --   "folke/noice.nvim",
  --   event = "VeryLazy",
  --   opts = {
  --     -- add any options here
  --   },
  --   dependencies = {
  --     -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
  --     "MunifTanjim/nui.nvim",
  --     -- OPTIONAL:
  --     --   `nvim-notify` is only needed, if you want to use the notification view.
  --     --   If not available, we use `mini` as the fallback
  --     "rcarriga/nvim-notify",
  --   },
  --   config = function()
  --     require("noice").setup({
  --       lsp = {
  --         -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
  --         override = {
  --           ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
  --           ["vim.lsp.util.stylize_markdown"] = true,
  --           ["cmp.entry.get_documentation"] = true,
  --         },
  --       },
  --       -- you can enable a preset for easier configuration
  --       presets = {
  --         bottom_search = true, -- use a classic bottom cmdline for search
  --         command_palette = true, -- position the cmdline and popupmenu together
  --         long_message_to_split = true, -- long messages will be sent to a split
  --         inc_rename = false, -- enables an input dialog for inc-rename.nvim
  --         lsp_doc_border = false, -- add a border to hover docs and signature help
  --       },
  --     })
  --   end,
  -- },
  -- the colorscheme should be available when starting Neovim
  {
    "folke/tokyonight.nvim",
    lazy = false,    -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
      -- load the colorscheme here
      vim.cmd([[colorscheme tokyonight]])
    end,
  },
  {
    "max397574/better-escape.nvim",
    config = function()
      require("better_escape").setup()
    end,
  },
  {

    "altermo/ultimate-autopair.nvim",
    event = { "InsertEnter", "CmdlineEnter" },
    branch = "v0.6",
    opts = {
      --Config goes here
    },
  },
  -- add this to your lua/plugins.lua, lua/plugins/init.lua,  or the file you keep your other plugins:
  {
    "numToStr/Comment.nvim",
    opts = function()
      -- add any options here
    end,
    lazy = false,
  },
  {
    "kevinhwang91/nvim-hlslens",
    config = function()
      require("hlslens").setup({
        -- calm_down = true,
        -- nearest_only = true,
        -- nearest_float_when = "always",
        -- override_lens = function(render, posList, nearest, idx, relIdx)
        --   local sfw = vim.v.searchforward == 1
        --   local indicator, text, chunks
        --   local absRelIdx = math.abs(relIdx)
        --   if absRelIdx > 1 then
        --     indicator = ("%d%s"):format(absRelIdx, sfw ~= (relIdx > 1) and "▲" or "▼")
        --   elseif absRelIdx == 1 then
        --     indicator = sfw ~= (relIdx == 1) and "▲" or "▼"
        --   else
        --     indicator = ""
        --   end
        --
        --   if posList[idx] ~= nil then
        --     local lnum, col = table.unpack(posList[idx])
        --     if nearest then
        --       local cnt = #posList
        --       if indicator ~= "" then
        --         text = ("[%s %d/%d]"):format(indicator, idx, cnt)
        --       else
        --         text = ("[%d/%d]"):format(idx, cnt)
        --       end
        --       chunks = { { " ", "Ignore" }, { text, "HlSearchLensNear" } }
        --     else
        --       text = ("[%s %d]"):format(indicator, idx)
        --       chunks = { { " ", "Ignore" }, { text, "HlSearchLens" } }
        --     end
        --     render.setVirt(0, lnum - 1, col - 1, chunks, nearest)
        --   end
        -- end,
      })
    end,
  },
  {
    "kevinhwang91/nvim-ufo",
    dependencies = { "kevinhwang91/promise-async" },
    event = "VeryLazy",
    init = function()
      vim.o.foldcolumn = "1" -- '0' is not bad
      vim.o.foldlevel = 99   -- Using ufo provider need a large value, feel free to decrease the value
      vim.o.foldlevelstart = 99
      vim.o.foldenable = true
    end,
    config = function()
      vim.keymap.set("n", "zR", require("ufo").openAllFolds)
      vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
      vim.keymap.set("n", "zK", require("ufo").peekFoldedLinesUnderCursor, { desc = "peek-folds" })
      vim.api.nvim_set_hl(0, "MoreMsg", { bg = "none", fg = "#7E9CD8" })

      local handler = function(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local totalLines = vim.api.nvim_buf_line_count(0)
        local foldedLines = endLnum - lnum
        local suffix = ("  %d %d%%"):format(foldedLines, foldedLines / totalLines * 100)
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        for _, chunk in ipairs(virtText) do
          local chunkText = chunk[1]
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
              suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
            end
            break
          end
          curWidth = curWidth + chunkWidth
        end
        local rAlignAppndx =
            math.max(math.min(vim.opt.textwidth["_value"], width - 1) - curWidth - sufWidth, 0)
        suffix = (" "):rep(rAlignAppndx) .. suffix
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end

      require("ufo").setup({
        -- provider_selector = function(bufnr, filetype, buftype)
        --   return { "treesitter", "indent" }
        -- end,
        fold_virt_text_handler = handler,
      })
    end,
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      highlight = {
        before = "",
        keyword = "fg",
        after = "",
      },
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
  },
  {
    "tamago324/lir.nvim",
    dependencies = {
      { "nvim-lua/plenary.nvim" },
      {
        "tamago324/lir-git-status.nvim",
        config = function()
          require("lir.git_status").setup({
            show_ignored = false,
          })
        end,
      },
    },
    init = function()
      -- disable netrw
      vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1

      vim.api.nvim_create_autocmd({ "FileType" }, {
        pattern = { "lir" },
        callback = function()
          -- use visual mode
          vim.api.nvim_buf_set_keymap(
            0,
            "x",
            "J",
            ':<C-u>lua require"lir.mark.actions".toggle_mark("v")<CR>',
            { noremap = true, silent = true }
          )

          -- echo cwd
          vim.api.nvim_echo({ { vim.fn.expand("%:p"), "Normal" } }, false, {})
        end,
      })
    end,
    keys = {
      { "<leader>lr", ":lua require'lir.float'.toggle()<CR>", desc = "lir explorer" },
    },
    config = function()
      -- local lir = require('lir')
      local actions = require("lir.actions")
      local marks = require("lir.mark.actions")
      local clipboard = require("lir.clipboard.actions")
      require("lir").setup({
        mappings = {
          ["q"] = actions.quit,
          ["o"] = actions.mkdir,
          ["m"] = marks.toggle_mark,
          ["cc"] = clipboard.copy,
          ["cx"] = clipboard.cut,
          ["cv"] = clipboard.paste,
        },
        float = {
          winblend = 0,
          win_opts = function()
            return {
              border = "single",
              zindex = 46,
            }
          end,
        },
      })
    end,
  },
  {
    "ggandor/flit.nvim",
    dependencies = {
      { "ggandor/leap.nvim" },
    }
  },
  {
	'mbbill/undotree',
    keys = {
      {"<leader>u", "vim.cmd.UndotreeToggle"}
    },
	-- config = function()
	-- 	vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
	-- end
}
  -- visual representation for idention
}
