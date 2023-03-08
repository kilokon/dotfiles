-- loaded_node_provider = 0,

local options = {
        -- Using wider tabs
        number         = true,
        relativenumber = true,
        shiftwidth     = 8,
        softtabstop    = 8,
        tabstop        = 8,
        expandtab      = true,
        smartindent    = true,
        breakindent    = true,
        signcolumn     = 'yes',
        -- backup & autosave
        -- 'backup' 'writebackup'	action	~
        -- off	     off	no backup made
        -- off	     on		backup current file, deleted afterwards (default)
        -- on	     off	delete old backup, backup current file
        -- on	     on		delete old backup, backup current file
        backup         = false,
        writebackup    = false,
        swapfile       = false,
        -- completeopt = { "menuone", "noselect" }, -- Options for insert mode completion
        -- copyindent = true, -- Copy the previous indentation on autoindenting
        cursorline     = true, -- Highlight the text line of the cursor
        pumheight      = 17, -- Height of the pop up menu
        -- mouse
        mouse          = "a",
        scrolloff      = 7, -- Minimal number of screen lines to keep above and below the cursor
        -- ui
        numberwidth    = 4,
        -- text
        list           = true,
        listchars      = { tab = '▸ ', trail = '·' },
        encoding       = "utf-8",
        ignorecase     = true,
        clipboard      = "unnamedplus",
        --hover
        --hover_with_actions = true,

        -- Undo
        undolevels     = 1000,
        undodir        = "$HOME/.cache/nvim/undo_ar", --table.concat(vim.fn.stdpath("data"), "undo", "/"),
        undofile       = true,
        -- Cmd
        cmdheight      = 0, -- use 2 for Wider command height
        shell          = "/usr/bin/zsh"
}

for k, v in pairs(options) do
        vim.opt[k] = v
end

local g = {
        highlighturl_enabled = true, -- highlight URLs by default
        autopairs_enabled = true, -- enable autopairs at start
        diagnostics_enabled = true, -- enable diagnostics at start
        status_diagnostics_enabled = true, -- enable diagnostics in statusline
        icons_enabled = true, -- disable icons in the UI (disable if no nerd font is available)
        ui_notifications_enabled = true, -- disable notifications when toggling UI elements
        loaded_perl_provider = 0,
        loaded_ruby_provider = 0,
}


for kk, vv in pairs(g) do
        vim.g[kk] = vv
end


--comletion
vim.opt.completeopt = { "menuone", "noselect", "noinsert" }
vim.opt.shortmess:append("c")

--vim.wo.whichwrap+=<,>,h,l
-- vim.opt.whichwrap = vim.opt.whichwrap + { '<', '>', }

vim.opt.shortmess = vim.opt.shortmess + { c = true }
vim.api.nvim_set_option('updatetime', 300)



-- Treesitter folding
vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
-- vim.wo.foldmethod = 'marker'
-- Rust Related
--
--
--
vim.g.rustc_path = "/usr/bin/rustc"
-- vim.g.rust_conceal = 1
vim.g.rust_fold = 1
vim.g.rustfmt_command = 'rustfmt'
