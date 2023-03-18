-- Reserve space for diagnostic icons
-- vim.opt.signcolumn = 'yes'

local lsp = require('lsp-zero')
local navic = require("nvim-navic")

navic.setup {
        icons = {
                File          = " ",
                Module        = " ",
                Namespace     = " ",
                Package       = " ",
                Class         = " ",
                Method        = " ",
                Property      = " ",
                Field         = " ",
                Constructor   = " ",
                Enum          = "練",
                Interface     = "練",
                Function      = " ",
                Variable      = " ",
                Constant      = " ",
                String        = " ",
                Number        = " ",
                Boolean       = "◩ ",
                Array         = " ",
                Object        = " ",
                Key           = " ",
                Null          = "ﳠ ",
                EnumMember    = " ",
                Struct        = " ",
                Event         = " ",
                Operator      = " ",
                TypeParameter = " ",
        },
        highlight = false,
        separator = " > ",
        depth_limit = 0,
        depth_limit_indicator = "..",
        safe_output = true
}



lsp.preset('recommended')


-- don't initialize this language server
-- we will use rust-tools to setup rust_analyzer
--local cmp_autopairs = require('nvim-autopairs.completion.cmp')
local luasnip = require("luasnip")
local cmp = require('cmp')
--cmp.event:on(
--        'confirm_done',
--        cmp_autopairs.on_confirm_done()
--)
lsp.setup_nvim_cmp({
        preselect = 'none',
        mapping = {
                ["<Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                                cmp.select_next_item()
                        elseif luasnip.jumpable(1) then
                                luasnip.jump(1)
                        else
                                fallback()
                        end
                end),
                ["<S-Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                                cmp.select_prev_item()
                        elseif luasnip.jumpable(-1) then
                                luasnip.jump(-1)
                        else
                                fallback()
                        end
                end),
                ["<cr>"] = cmp.mapping.confirm({
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = false,
                }),
        },
        sources = {
                { name = "luasnip" },
                { name = "nvim_lsp" },
                { name = "crates" },
                { name = "buffer" },
                { name = "path" },
        },
        experimental = {
                native_menu = false,
                ghost_text = true,
        },
})
local wk = require("which-key")
-- local opts_n = require("drystuff.noremaps").map_options("n")

-- local opts = { noremap = true, silent = true }
-- vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
-- vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
-- vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
-- vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)




lsp.skip_server_setup({ 'rust_analyzer' })
lsp.on_attach(function(client, bufnr)
        if client.server_capabilities.documentSymbolProvider then
                navic.attach(client, bufnr)
        end
        require("lsp-format").on_attach(client)
        local mapping = {
                f = {
                        name = "Files",
                        a = { vim.lsp.buf.add_workspace_folder, "Add Workspace Folder" },
                        d = { vim.lsp.buf.remove_workspace_folder, "Remove Workspace Folder" },
                        R = { vim.lsp.buf.rename(), "Format" },
                },
                l = {
                        name = "LSP",
                        a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
                        f = { "<cmd> lua vim.lsp.buf.type_definition()<cr>", "Type Definition" },
                        k = { "<cmd>lua vim.lsp.signature_help()<cr>", "Signature" },
                        -- i = { "<cmd>LspInfo<cr>", "Info" },
                        -- I = { "<cmd>Mason<cr>", "Mason Info" },
                        D = {
                                name = "Diagnostics",
                                d = { "<cmd>Telescope diagnostics bufnr=0 theme=get_ivy<cr>", "Buffer Diagnostics" },
                                e = { "<cmd>Telescope quickfix<cr>", "Telescope Quickfix" },
                                q = { "<cmd>lua vim.diagnostic.setloclist()<cr>", "Quickfix" },
                                w = { "<cmd>Telescope diagnostics<cr>", "Diagnostics" },
                                j = {
                                        "<cmd>lua vim.diagnostic.goto_next()<cr>",
                                        "Next Diagnostic",
                                },
                                k = {
                                        "<cmd>lua vim.diagnostic.goto_prev()<cr>",
                                        "Prev Diagnostic",
                                },
                        },
                        l = { "<cmd>lua vim.lsp.codelens.run()<cr>", "CodeLens Action" },
                        -- r = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
                        s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
                        S = {
                                "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
                                "Workspace Symbols",
                        },
                },
        }


        -- local opts = { buffer = bufnr }
        -- local bind = vim.keymap.set
        -- Enable completion triggered by <c-x><c-o>
        --vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
        -- bind('n', '<leader>r', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
        local bufopts = { noremap = true, silent = true, buffer = bufnr }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
        vim.keymap.set('n', '<space>m', vim.lsp.buf.type_definition, bufopts)
        -- vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
        vim.keymap.set('n', "C-k", "<Nop>", bufopts)
        -- vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
        -- vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
        -- vim.keymap.set('n', '<space>wl', function()
        --         print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        -- end, bufopts)
        -- vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
        -- vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
        -- vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
        -- vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
        -- vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
        wk.register(mapping, { prefix = "<leader>" })
end)

-- lsp.configure('pyright', {
--         on_attach = function(_, bufnr)
--                 -- custom_attach(client, bufnr)
--                 -- 'Organize imports' keymap for pyright only
--                 vim.keymap.set("n", "<Leader>ii", "<cmd>PyrightOrganizeImports<CR>", {
--                         buffer = bufnr,
--                         silent = true,
--                         noremap = true,
--                 })
--                 -- require("lsp-format").on_attach(client)
--         end,
--         settings = {
--                 pyright = {
--                         disableOrganizeImports = false,
--                         analysis = {
--                                 useLibraryCodeForTypes = true,
--                                 autoSearchPaths = true,
--                                 diagnosticMode = "workspace",
--                                 autoImportCompletions = true,
--                         },
--                 },
--         },
-- })
--Pass arguments to a language server
-- lsp.configure('sumneko_lua', {
--         on_attach = function(client, bufnr)
--                 print('hello sumneko')
--                 -- require("lsp-format").on_attach(client)
--         end,
--         settings = {
--                 Lua = {
--                         diagnostics = {
--                                 globals = { 'vim' }
--                         }
--                 },
--                 completions = {
--                         completeFunctionCalls = true
--                 }
--         }
-- })




--Configure lua language server for neovim
lsp.nvim_workspace()

lsp.setup()
vim.diagnostic.config({
        virtual_text = false,
        signs = false,     -- change this
        update_in_insert = false,
        underline = false, -- change this
        severity_sort = false,
        float = false,     -- change this
})



local lua_lsp = lsp.build_options('lua_language_server', {})

require("neodev").setup({
        override = lua_lsp
        -- add any options here, or leave empty to use the default settings
})




-- Initialize rust_analyzer with rust-tools
local rust_lsp = lsp.build_options('rust_analyzer', {})

local extension_path = vim.env.HOME .. '/.vscode-oss/extensions/vadimcn.vscode-lldb-1.8.1-universal/'
local codelldb_path = extension_path .. 'adapter/codelldb'
local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'
require('rust-tools').setup({
        server = {
                rust_lsp,
                on_attach = function(_, bufnr)
                        local rt = require "rust-tools"
                        vim.keymap.set("n", "<f6>", "<cmd>BaconLoad<CR>:w<CR>BaconNext<CR>", { buffer = bufnr })
                        vim.keymap.set("n", "<f5>", "<cmd>BaconShow<CR>", { buffer = bufnr })
                        -- Hover actions
                        vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
                        -- Code action groups
                        -- vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
                        -- Rust Runables
                        vim.keymap.set("n", "rr", rt.runnables.runnables, { buffer = bufnr })
                        -- Rust Cargo Open
                        vim.keymap.set("n", "rc", rt.open_cargo_toml.open_cargo_toml, { buffer = bufnr })
                end,
                settings = {
                        -- to enable rust-analyzer settings visit:
                        -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
                        ["rust-analyzer"] = {
                                -- enable clippy on save
                                checkOnSave = {
                                        command = "clippy"
                                }
                        }
                },
        },
        tools = {
                autoSetHints = true,
                --hover_with_actions = true,
                executor = require("rust-tools/executors").termopen,
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
                        parameter_hints_prefix = "<- ",
                        -- prefix for all the other hints (type, chaining)
                        -- default: "=>"
                        other_hints_prefix = "=> ",
                        -- whether to align to the length of the longest line in the file
                        max_len_align = false,
                        -- padding from the left if max_len_align is true
                        max_len_align_padding = 1,
                        -- whether to align to the extreme right or not
                        right_align = false,
                        -- padding from the right if right_align is true
                        right_align_padding = 7,
                        -- The color of the hints
                        highlight = "Comment",
                },
                -- inlay_hints = {
                --         only_current_line = false,
                --         only_current_line_autocmd = "CursorHold",
                --         show_parameter_hints = true,
                --         show_variable_name = true,
                --         parameter_hints_prefix = "<- ",
                --         other_hints_prefix = "=> ",
                --         max_len_align = false,
                --         max_len_align_padding = 1,
                --         right_align = false,
                --         right_align_padding = 7,
                --         highlight = "Comment",
                -- },
                hover_actions = {
                        border = {
                                { "╭", "FloatBorder" },
                                { "─", "FloatBorder" },
                                { "╮", "FloatBorder" },
                                { "│", "FloatBorder" },
                                { "╯", "FloatBorder" },
                                { "─", "FloatBorder" },
                                { "╰", "FloatBorder" },
                                { "│", "FloatBorder" },
                        },
                        auto_focus = true,
                },
        },
        dap = {
                adapter = require('rust-tools.dap').get_codelldb_adapter(
                        codelldb_path, liblldb_path)
        }
})


-- map("n", "C-k", "<Nop>", opts)


-- Format on save
