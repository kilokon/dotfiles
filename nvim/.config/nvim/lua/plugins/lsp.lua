-- Reserve space for diagnostic icons
-- vim.opt.signcolumn = 'yes'

vim.diagnostic.config {
        virtual_text = false,
        signs = true,     -- change this
        update_in_insert = false,
        underline = true, -- change this
        severity_sort = false,
        float = {
                scope = 'cursor',
                border = 'single',
                header = '',
                prefix = '',
                focusable = false,
        },
        --float = false,     -- change this
}




-- K: Displays hover information about the symbol under the cursor in a floating window. See :help vim.lsp.buf.hover().
--
-- gd: Jumps to the definition of the symbol under the cursor. See :help vim.lsp.buf.definition().
--
-- gD: Jumps to the declaration of the symbol under the cursor. Some servers don't implement this feature. See :help vim.lsp.buf.declaration().
--
-- gi: Lists all the implementations for the symbol under the cursor in the quickfix window. See :help vim.lsp.buf.implementation().
--
-- go: Jumps to the definition of the type of the symbol under the cursor. See :help vim.lsp.buf.type_definition().
--
-- gr: Lists all the references to the symbol under the cursor in the quickfix window. See :help vim.lsp.buf.references().
--
-- gs: Displays signature information about the symbol under the cursor in a floating window. See :help vim.lsp.buf.signature_help(). If a mapping already exists for this key this function is not bound.
--
-- <F2>: Renames all references to the symbol under the cursor. See :help vim.lsp.buf.rename().
--
-- <F3>: Format code in current buffer. See :help vim.lsp.buf.format().
--
-- <F4>: Selects a code action available at the current cursor position. See :help vim.lsp.buf.code_action().
--
-- gl: Show diagnostics in a floating window. See :help vim.diagnostic.open_float().
--
-- [d: Move to the previous diagnostic in the current buffer. See :help vim.diagnostic.goto_prev().
--
-- ]d: Move to the next diagnostic. See :help vim.diagnostic.goto_next().




local lsp = require 'lsp-zero'
vim.lsp.set_log_level("debug")
lsp.preset {
        name = 'recommended',
        set_lsp_keymaps = false,
        suggest_lsp_servers = false,
        manage_nvim_cmp = {
                set_basic_mapping = true,
                documentation_window = true,
        }
}

local navic = require 'nvim-navic'

vim.o.foldcolumn = '1'
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

-- Using ufo provider need remap `zR` and `zM`.
vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)

-- ufo folding provider
require('ufo').setup()

-- inlay hints
local ih = require('inlay-hints')
ih.setup({
        -- renderer to use
        -- possible options are dynamic, eol, virtline and custom
        renderer = "inlay-hints/render/eol",
        hints = {
                parameter = {
                        show = true,
                        highlight = "Comment",
                },
                type = {
                        show = true,
                        highlight = "Comment",
                },
        },
        only_current_line = false,
        eol = {
                right_align = false,
        }
})

navic.setup {
        icons = {
                File = ' ',
                Module = ' ',
                Namespace = ' ',
                Package = ' ',
                Class = ' ',
                Method = ' ',
                Property = ' ',
                Field = ' ',
                Constructor = ' ',
                Enum = '練',
                Interface = '練',
                Function = ' ',
                Variable = ' ',
                Constant = ' ',
                String = ' ',
                Number = ' ',
                Boolean = '◩ ',
                Array = ' ',
                Object = ' ',
                Key = ' ',
                Null = 'ﳠ ',
                EnumMember = ' ',
                Struct = ' ',
                Event = ' ',
                Operator = ' ',
                TypeParameter = ' ',
        },
        highlight = false,
        separator = ' > ',
        depth_limit = 0,
        depth_limit_indicator = '..',
        safe_output = true,
}

-- lsp.preset 'recommended'

local luasnip = require 'luasnip'
local cmp = require 'cmp'
--cmp.event:on(
--        'confirm_done',
--        cmp_autopairs.on_confirm_done()
--)
lsp.setup_nvim_cmp {
        preselect = 'none',
        window = {
                completion = cmp.config.window.bordered(),
                documentation = cmp.config.window.bordered(),
        },
        mapping = {
                ['<Tab>'] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                                cmp.select_next_item()
                        elseif luasnip.jumpable(1) then
                                luasnip.jump(1)
                        else
                                fallback()
                        end
                end),
                ['<S-Tab>'] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                                cmp.select_prev_item()
                        elseif luasnip.jumpable(-1) then
                                luasnip.jump(-1)
                        else
                                fallback()
                        end
                end),
                ['<cr>'] = cmp.mapping.confirm {
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = false,
                },
        },
        sources = {
                { name = 'luasnip' },
                { name = 'nvim_lsp' },
                { name = 'crates' },
                { name = 'buffer' },
                { name = 'path' },
        },
        experimental = {
                native_menu = false,
                ghost_text = true,
        },
}
-- don't initialize this language server
-- we will use rust-tools to setup rust_analyzer

local function common_on_attach(_, bufnr)
        local lsp_hover_augroup = vim.api.nvim_create_augroup('config_lsp_hover', { clear = false })
        vim.api.nvim_clear_autocmds {
                buffer = bufnr,
                group = lsp_hover_augroup,
        }
        vim.api.nvim_create_autocmd('CursorHold', {
                callback = function()
                        for _, winid in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
                                if vim.api.nvim_win_get_config(winid).relative ~= '' then
                                        return
                                end
                        end
                        vim.diagnostic.open_float()
                end,
                group = lsp_hover_augroup,
                buffer = bufnr,
        })
end

local function allow_format(servers)
        return function(client) return vim.tbl_contains(servers, client.name) end
end

lsp.on_attach(function(client, bufnr)
        lsp.default_keymaps { buffer = bufnr }
        lsp.buffer_autoformat()
        local opts = { buffer = bufnr, remap = false }
        local additional_opts = { buffer = bufnr, remap = false, silent = true }
        -- vim.keymap.set('n', 'gd', function() vim.lsp.buf.definition() end, opts)
        vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
        vim.keymap.set('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
        -- vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
        vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)
        vim.keymap.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
        vim.keymap.set('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts)
        vim.keymap.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
        vim.keymap.set('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
        vim.keymap.set('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
        -- vim.keymap.set({ 'n', 'x' }, '<F3>', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', opts)
        vim.keymap.set('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)
        vim.keymap.set('n', '<leader>rx', function() require('telescope.builtin').lsp_references() end, additional_opts)

        vim.keymap.set({ 'n', 'x' }, 'gq', function()
                vim.lsp.buf.format({
                        async = false,
                        timeout_ms = 10000,
                        filter = allow_format({ 'lua_ls', 'rust_analyzer' })
                })
        end, opts)

        if client.server_capabilities.documentSymbolProvider then
                require('nvim-navic').attach(client, bufnr)
        end

        common_on_attach(client, bufnr)
end)

lsp.configure('typst_lsp', {
        cmd = { 'typst_lsp' },
        filetypes = { 'typst' },
        on_attach = function(_, _)
                print 'typst attached'
                --common_on_attach(client, bufnr)
                --               require('lsp-format').on_attach(client)
        end,
})

lsp.configure('taplo', {
        cmd = { 'taplo', 'lsp', 'stdio' },
        filetypes = { 'toml' },
        on_attach = function(_, _)
                print 'taplo attached'
                --common_on_attach(client, bufnr)
                --               require('lsp-format').on_attach(client)
        end,
})


lsp.configure('clangd', {
        cmd = { 'clangd', '--background-index', '--clang-tidy', '--completion-style=bundled', '--header-insertion=iwyu' },
        filetypes = { 'c', 'cpp', 'objc', 'objcpp' },
        on_attach = function(_, _)
                print 'clangd attached'
                --common_on_attach(client, bufnr)
                --               require('lsp-format').on_attach(client)
        end,
})

lsp.configure('pyright', {
        on_attach = function(_, bufnr)
                -- if client.server_capabilities.documentSymbolProvider then
                --         navic.attach(client, bufnr)
                -- end

                -- custom_attach(client, bufnr)
                -- 'Organize imports' keymap for pyright only
                vim.keymap.set('n', '<Leader>ii', '<cmd>PyrightOrganizeImports<CR>', {
                        buffer = bufnr,
                        silent = true,
                        noremap = true,
                })
                -- require("lsp-format").on_attach(client)
        end,
        settings = {
                pyright = {
                        disableOrganizeImports = false,
                        analysis = {
                                useLibraryCodeForTypes = true,
                                autoSearchPaths = true,
                                diagnosticMode = 'workspace',
                                autoImportCompletions = true,
                        },
                },
        },
})


lsp.configure('lua_ls', {
        root_dir = function()
                --- project root will be the first directory that has
                --- either .luarc.json or .stylua.toml
                return lsp.dir.find_first({ '.luarc.json', '.stylua.toml' })
        end,
        on_attach = function(client, bufnr)
                --                print "hello from lua_ls"
                ih.on_attach(client, bufnr)
        end,
        settings = {
                Lua = {
                        hint = {
                                enable = true,
                        },
                        diagnostics = {
                                globals = { 'vim' },
                        },
                },
        },
})

lsp.set_server_config({
        capabilities = {
                textDocument = {
                        foldingRange = {
                                dynamicRegistration = false,
                                lineFoldingOnly = true
                        }
                }
        }
})

--Configure lua language server for neovim
-- lsp.nvim_workspace()
lsp.skip_server_setup { 'rust_analyzer', 'hls' }

lsp.setup()


-- Initialize rust_analyzer with rust-tools
local rust_lsp = lsp.build_options('rust_analyzer', {})

local extension_path = vim.env.HOME .. '/.vscode-oss/extensions/vadimcn.vscode-lldb-1.8.1-universal/'
local codelldb_path = extension_path .. 'adapter/codelldb'
local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'
require('rust-tools').setup {
        server = {
                rust_lsp,
                on_attach = function(client, bufnr)
                        -- if client.server_capabilities.documentSymbolProvider then
                        --         navic.attach(client, bufnr)
                        -- end
                        common_on_attach(client, bufnr)
                        local rt = require 'rust-tools'
                        vim.keymap.set('n', '<f6>', '<cmd>BaconLoad<CR>:w<CR>BaconNext<CR>', { buffer = bufnr })
                        vim.keymap.set('n', '<f5>', '<cmd>BaconShow<CR>', { buffer = bufnr })
                        -- Hover actions
                        vim.keymap.set('n', '<C-space>', rt.hover_actions.hover_actions, { buffer = bufnr })
                        -- Code action groups
                        -- vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
                        -- Rust Runables
                        vim.keymap.set('n', 'rr', rt.runnables.runnables, { buffer = bufnr })
                        -- Rust Cargo Open
                        vim.keymap.set('n', 'rc', rt.open_cargo_toml.open_cargo_toml, { buffer = bufnr })
                        ih.on_attach(client, bufnr) -- Inlayhints
                end,
                settings = {
                        -- to enable rust-analyzer settings visit:
                        -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
                        ['rust-analyzer'] = {
                                -- enable clippy on save
                                checkOnSave = {
                                        command = 'clippy',
                                },
                        },
                },
        },
        tools = {
                autoSetHints = true,
                --hover_with_actions = true,
                executor = require('rust-tools/executors').termopen,
                on_initialized = function()
                        ih.set_all()
                end,
                -- automatically call RustReloadWorkspace when writing to a Cargo.toml file.
                reload_workspace_from_cargo_toml = true,
                inlay_hints = {
                        -- automatically set inlay hints (type hints)
                        -- default: true
                        auto = false,
                        -- Only show inlay hints for the current line
                        only_current_line = false,
                        -- whether to show parameter hints with the inlay hints or not
                        -- default: true
                        show_parameter_hints = true,
                        show_variable_name = true,
                        -- prefix for parameter hints
                        -- default: "<-"
                        parameter_hints_prefix = '<- ',
                        -- prefix for all the other hints (type, chaining)
                        -- default: "=>"
                        other_hints_prefix = '=> ',
                        -- whether to align to the length of the longest line in the file
                        max_len_align = false,
                        -- padding from the left if max_len_align is true
                        max_len_align_padding = 1,
                        -- whether to align to the extreme right or not
                        right_align = false,
                        -- padding from the right if right_align is true
                        right_align_padding = 7,
                        -- The color of the hints
                        highlight = 'Comment',
                },
                hover_actions = {
                        border = {
                                { '╭', 'FloatBorder' },
                                { '─', 'FloatBorder' },
                                { '╮', 'FloatBorder' },
                                { '│', 'FloatBorder' },
                                { '╯', 'FloatBorder' },
                                { '─', 'FloatBorder' },
                                { '╰', 'FloatBorder' },
                                { '│', 'FloatBorder' },
                        },
                        auto_focus = true,
                },
        },
        dap = {
                adapter = require('rust-tools.dap').get_codelldb_adapter(codelldb_path, liblldb_path),
        },
}

local def_opts = { noremap = true, silent = true }
local haskell_tools = require('haskell-tools')
local hls_lsp = require('lsp-zero').build_options('hls', {})

local hls_config = {
        tools = {
                -- haskell-tools options
                codeLens = {
                        -- Whether to automatically display/refresh codeLenses
                        -- (explicitly set to false to disable)
                        autoRefresh = true,
                },
                hoogle = {
                        -- 'auto': Choose a mode automatically, based on what is available.
                        -- 'telescope-local': Force use of a local installation.
                        -- 'telescope-web': The online version (depends on curl).
                        -- 'browser': Open hoogle search in the default browser.
                        mode = 'auto',
                },
                hover = {
                        -- Whether to disable haskell-tools hover
                        -- and use the builtin lsp's default handler
                        disable = false,
                        -- Set to nil to disable
                        border = {
                                { '╭', 'FloatBorder' },
                                { '─', 'FloatBorder' },
                                { '╮', 'FloatBorder' },
                                { '│', 'FloatBorder' },
                                { '╯', 'FloatBorder' },
                                { '─', 'FloatBorder' },
                                { '╰', 'FloatBorder' },
                                { '│', 'FloatBorder' },
                        },
                        -- Stylize markdown (the builtin lsp's default behaviour).
                        -- Setting this option to false sets the file type to markdown and enables
                        -- Treesitter syntax highligting for Haskell snippets
                        -- if nvim-treesitter is installed
                        stylize_markdown = false,
                        -- Whether to automatically switch to the hover window
                        auto_focus = false,
                },
                definition = {
                        -- Configure vim.lsp.definition to fall back to hoogle search
                        -- (does not affect vim.lsp.tagfunc)
                        hoogle_signature_fallback = false,
                },
                repl = {
                        -- 'builtin': Use the simple builtin repl
                        -- 'toggleterm': Use akinsho/toggleterm.nvim
                        handler = 'builtin',
                        builtin = {
                                create_repl_window = function(view)
                                        -- create_repl_split | create_repl_vsplit | create_repl_tabnew | create_repl_cur_win
                                        return view.create_repl_vsplit()
                                end
                        },
                        -- Can be overriden to either `true` or `false`.
                        -- The default behaviour depends on the handler.
                        auto_focus = true,
                        delete_buffer_on_exit = true,
                },
                -- Set up autocmds to generate tags (using fast-tags)
                -- e.g. so that `vim.lsp.tagfunc` can fall back to Haskell tags
                tags = {
                        enable = vim.fn.executable('fast-tags') == 1,
                        -- Events to trigger package tag generation
                        package_events = { 'BufWritePost' },
                },
                dap = {
                        cmd = { 'haskell-debug-adapter' },
                },
        },
        hls = {
                capabilities = hls_lsp.capabilities,
                on_attach = function(_, bufnr)
                        local opts = { buffer = bufnr }

                        -- haskell-language-server relies heavily on codeLenses,
                        -- so auto-refresh (see advanced configuration) is enabled by default
                        vim.keymap.set('n', '<leader>ca', vim.lsp.codelens.run, opts)
                        vim.keymap.set('n', '<leader>hs', haskell_tools.hoogle.hoogle_signature, opts)
                        vim.keymap.set('n', '<leader>ea', haskell_tools.lsp.buf_eval_all, opts)
                end
        }
}

-- Autocmd that will actually be in charging of starting hls
local hls_augroup = vim.api.nvim_create_augroup('haskell-lsp', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
        group = hls_augroup,
        pattern = { 'haskell' },
        callback = function(_, bufnr)
                local opts = { buffer = bufnr, remap = false }
                haskell_tools.start_or_attach(hls_config)

                ---
                -- Suggested keymaps that do not depend on haskell-language-server:
                ---

                -- Toggle a GHCi repl for the current package
                vim.keymap.set('n', '<leader>rr', haskell_tools.repl.toggle, opts)

                -- Toggle a GHCi repl for the current buffer
                vim.keymap.set('n', '<leader>rf', function()
                        haskell_tools.repl.toggle(vim.api.nvim_buf_get_name(0))
                end, def_opts)

                vim.keymap.set('n', '<leader>rq', haskell_tools.repl.quit, opts)
        end
})





--
vim.g.copilot_no_tab_map = true
vim.g.copilot_assume_mapped = true
vim.api.nvim_set_keymap('i', '<C-l>', 'copilot#Accept("<CR>")', { silent = true, expr = true })
vim.api.nvim_set_keymap('i', '<M-]>', 'copilot#Next()', { silent = true, expr = true })
vim.api.nvim_set_keymap('i', '<M-x>', 'copilot#Disable()', { silent = true, expr = true })
-- vim.keymap.set('', '<Leader>xx', require('lsp_lines').toggle, { desc = 'Toggle lsp_lines' })

-- map("n", "C-k", "<Nop>", opts)

-- Format on save
