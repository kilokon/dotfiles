-- Reserve space for diagnostic icons
-- vim.opt.signcolumn = 'yes'

local lsp = require 'lsp-zero'
local navic = require 'nvim-navic'

local null_ls = require 'null-ls'

local augroup = vim.api.nvim_create_augroup('LspFormatting', {})

local null_on_attach = function(client, bufnr)
        if client.supports_method 'textDocument/formatting' then
                vim.api.nvim_clear_autocmds { group = augroup, buffer = bufnr }
                vim.api.nvim_create_autocmd('BufWritePre', {
                        group = augroup,
                        buffer = bufnr,
                        callback = function()
                                vim.lsp.buf.format { bufnr = bufnr }
                                -- vim.lsp.buf.formatting_sync()
                        end,
                })
        end
end

null_ls.setup {
        sources = {
                -- null_ls.builtins.diagnostics.chktex,
                null_ls.builtins.hover.dictionary,
                null_ls.builtins.formatting.prettier,
                null_ls.builtins.formatting.ruff,
                null_ls.builtins.diagnostics.glslc.with {
                        extra_args = { '--target-env=opengl' }, -- use opengl instead of vulkan1.0
                },
                -- For formatting toml files
                null_ls.builtins.formatting.taplo,
                -- yq is a portable command-line YAML, JSON, XML, CSV and properties processor.
                null_ls.builtins.formatting.yq.with { filetypes = { 'csv', 'yaml', 'yml' } },
                null_ls.builtins.formatting.rustfmt,
                null_ls.builtins.formatting.stylua,
                null_ls.builtins.formatting.clang_format.with { command = 'clang-format-14' },
                null_ls.builtins.code_actions.gitsigns,

                -- Shows the value for the current environment variable under the cursor.
                null_ls.builtins.hover.printenv,
                -- null_ls.builtins.omnifunc,
        },
        debug = true,
        on_attach = null_on_attach,
}

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

lsp.preset 'recommended'

-- don't initialize this language server
-- we will use rust-tools to setup rust_analyzer
--local cmp_autopairs = require('nvim-autopairs.completion.cmp')
local luasnip = require 'luasnip'
local cmp = require 'cmp'
--cmp.event:on(
--        'confirm_done',
--        cmp_autopairs.on_confirm_done()
--)
lsp.setup_nvim_cmp {
        preselect = 'none',
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
local wk = require 'which-key'

local lsp_format = function(bufnr)
        vim.lsp.buf.format {
                filter = function(client)
                        -- only use null-ls
                        return client.name == 'null-ls'
                end,
                bufnr = bufnr,
                async = true,
        }
end

local function key_maps(bufnr)
        --Enable completion triggered by <c-x><c-o>
        -- vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

        -- Mappings.
        local opts = { buffer = bufnr, noremap = true, silent = true }

        local lspmaps = {
                ['<leader>l'] = {
                        name = 'Lsp',
                        a = { '<cmd>lua vim.lsp.buf.code_action()<CR>', 'Code Action' },
                        d = { "<cmd>lua require('telescope.builtin').lsp_definitions()<CR>", 'Definition' },
                        D = { '<cmd>lua vim.lsp.buf.declaration()<CR>', 'Declaration' },
                        -- fl = {
                        --         function()
                        --                 lsp_format(bufnr)
                        --         end,
                        --         'Format Buffer',
                        -- },
                        -- ff = { '<cmd>lua vim.lsp.buf.format({ async = false })<CR>', 'Format Buffer' },
                        i = { "<cmd>lua require('telescope.builtin').lsp_implementations()<CR>", 'Implementation' },
                        k = { '<cmd>lua vim.lsp.buf.hover()<CR>', 'Hover' },
                        h = { '<cmd>lua vim.lsp.buf.signature_help()<CR>', 'Help' },
                        n = { '<cmd>lua vim.lsp.buf.rename()<CR>', 'Rename' },
                        r = { "<cmd>lua require('telescope.builtin').lsp_references()<CR>", 'References' },
                        l = { "<cmd>lua vim.diagnostic.open_float({scope = 'line'})<CR>", 'Show Diagnostics' },
                },
                ['[d'] = { '<cmd>lua vim.diagnostic.goto_prev()<CR>', 'Prev Diagnostics' },
                [']d'] = { '<cmd>lua vim.diagnostic.goto_next()<CR>', 'Next Diagnostics' },
        }

        wk.register(lspmaps, opts)
end

lsp.skip_server_setup { 'rust_analyzer' }

local function common_on_attach(client, bufnr)
        key_maps(bufnr)
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
        print 'lsp attached'
        -- require('lsp-format').on_attach(client)
        if client.server_capabilities.documentSymbolProvider then
                print 'navic initiated'
                navic.attach(client, bufnr)
        end
end

lsp.on_attach(function(client, bufnr)
        common_on_attach(client, bufnr)
end)

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

--Configure lua language server for neovim
lsp.nvim_workspace()

lsp.setup()

vim.diagnostic.config {
        virtual_text = false,
        signs = true, -- change this
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
vim.cmd 'sign define DiagnosticSignError text= texthl=DiagnosticSignError'
vim.cmd 'sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn'
vim.cmd 'sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo'
vim.cmd 'sign define DiagnosticSignHint text= texthl=DiagnosticSignHint'

local lua_lsp = lsp.build_options('lua_language_server', {})

require('neodev').setup {
        override = { lua_lsp },
        -- add any options here, or leave empty to use the default settings
}

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
                on_initialized = nil,
                -- automatically call RustReloadWorkspace when writing to a Cargo.toml file.
                reload_workspace_from_cargo_toml = true,
                inlay_hints = {
                        -- automatically set inlay hints (type hints)
                        -- default: true
                        auto = true,
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

vim.keymap.set('', '<Leader>xx', require('lsp_lines').toggle, { desc = 'Toggle lsp_lines' })

-- map("n", "C-k", "<Nop>", opts)

-- Format on save
