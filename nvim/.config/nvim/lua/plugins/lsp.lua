-- Reserve space for diagnostic icons
vim.opt.signcolumn = 'yes'

local lsp = require('lsp-zero')
lsp.preset('recommended')

lsp.ensure_installed({
        -- Replace these with whatever servers you want to install
        'rust_analyzer',
        'pyright',
        --  'eslint',
        'sumneko_lua',
})

lsp.set_preferences({
        init_options = { documentFormatting = true },
})
-- don't initialize this language server
-- we will use rust-tools to setup rust_analyzer
lsp.skip_server_setup({ 'rust_analyzer' })

lsp.on_attach(function(client, bufnr)
        require("lsp-format").on_attach(client)
        local opts = { buffer = bufnr, remap = false }

        vim.keymap.set("n", "gd", function()
                vim.lsp.buf.definition()
        end, opts)
        vim.keymap.set("n", "K", function()
                vim.lsp.buf.hover()
        end, opts)
        vim.keymap.set("n", "<leader>ws", function()
                vim.lsp.buf.workspace_symbol()
        end, opts)
        vim.keymap.set("n", "<leader>of", function()
                vim.diagnostic.open_float()
        end, opts)
        vim.keymap.set("n", "[d", function()
                vim.diagnostic.goto_next()
        end, opts)
        vim.keymap.set("n", "]d", function()
                vim.diagnostic.goto_prev()
        end, opts)
        vim.keymap.set("n", "<leader>ca", function()
                vim.lsp.buf.code_action()
        end, opts)
        vim.keymap.set("n", "<leader>rf", function()
                vim.lsp.buf.references()
        end, opts)
        vim.keymap.set("n", "<leader>rn", function()
                vim.lsp.buf.rename()
        end, opts)
        vim.keymap.set("i", "<C-h>", function()
                vim.lsp.buf.signature_help()
        end, opts)
        print('Greetings from on_attach')
end)

lsp.configure('pyright', {
        on_attach = function(client, bufnr)
                -- custom_attach(client, bufnr)
                -- 'Organize imports' keymap for pyright only
                vim.keymap.set("n", "<Leader>ii", "<cmd>PyrightOrganizeImports<CR>", {
                        buffer = bufnr,
                        silent = true,
                        noremap = true,
                })
        end,
        settings = {
                pyright = {
                        disableOrganizeImports = false,
                        analysis = {
                                useLibraryCodeForTypes = true,
                                autoSearchPaths = true,
                                diagnosticMode = "workspace",
                                autoImportCompletions = true,
                        },
                },
        },
})
-- Pass arguments to a language server
-- lsp.configure('sumneko_lua', {
--         on_attach = function(client, bufnr)
--                 print('hello sumneko')
--         end,
--         settings = {
--                 completions = {
--                         completeFunctionCalls = true
--                 }
--         }
-- })
--
-- Configure lua language server for neovim
lsp.nvim_workspace()

lsp.setup()

-- Initialize rust_analyzer with rust-tools
local rust_lsp = lsp.build_options('rust_analyzer', {})

require('rust-tools').setup({
        server = rust_lsp,
        tools = {
                autoSetHints = true,
                --hover_with_actions = true,
                executor = require("rust-tools/executors").termopen,
                on_initialized = nil,
                inlay_hints = {
                        only_current_line = false,
                        only_current_line_autocmd = "CursorHold",
                        show_parameter_hints = true,
                        show_variable_name = false,
                        parameter_hints_prefix = "<- ",
                        other_hints_prefix = "=> ",
                        max_len_align = false,
                        max_len_align_padding = 1,
                        right_align = false,
                        right_align_padding = 7,
                        highlight = "Comment",
                },
        },

})

-- Format on save
