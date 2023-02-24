-- Reserve space for diagnostic icons
-- vim.opt.signcolumn = 'yes'

local lsp = require('lsp-zero')

lsp.preset('recommended')

-- lsp.ensure_installed({
--         -- Replace these with whatever servers you want to install
--         'rust_analyzer',
--         'clangd',
--         'pyright',
--         'openscad_lsp',
--         'taplo',
--         'bashls',
--         'eslint',
--         'emmet_ls',
--         'arduino_language_server',
--         -- 'sumneko_lua',
-- })
--
-- require("lsp-format").setup {}

-- lsp.set_preferences({
--         init_options = { documentFormatting = true },
-- })


local ELLIPSIS_CHAR = '…'
local MAX_LABEL_WIDTH = 25
-- local MAX_KIND_WIDTH = 14

-- local get_ws = function(max, len)
--         return (" "):rep(max - len)
-- end
-- local menu_icon = {
--         Text = "   (Text) ",
--         Method = "   (Method)",
--         Function = "   (Function)",
--         Constructor = "   (Constructor)",
--         Field = " ﴲ  (Field)",
--         Variable = "[] (Variable)",
--         Class = "   (Class)",
--         Interface = " ﰮ  (Interface)",
--         Module = "   (Module)",
--         Property = " 襁 (Property)",
--         Unit = "   (Unit)",
--         Value = "   (Value)",
--         Enum = " 練 (Enum)",
--         Keyword = "   (Keyword)",
--         Snippet = "   (Snippet)",
--         Color = "   (Color)",
--         File = "   (File)",
--         Reference = "   (Reference)",
--         Folder = "   (Folder)",
--         EnumMember = "   (EnumMember)",
--         Constant = " ﲀ  (Constant)",
--         Struct = " ﳤ  (Struct)",
--         Event = "   (Event)",
--         Operator = "   (Operator)",
--         TypeParameter = "   (TypeParameter)",
-- }
--
-- local kind_icons = {
--     ["Class"] = "🅒 ",
--     ["Interface"] = "🅘 ",
--     ["TypeParameter"] = "🅣 ",
--     ["Struct"] = "🅢",
--     ["Enum"] = "🅔 ",
--     ["Unit"] = "🅤 ",
--     ["EnumMember"] = "🅔 ",
--     ["Constant"] = "🅒 ",
--     ["Field"] = "🅕 ",
--     ["Property"] = " ",
--     ["Variable"] = "🅥 ",
--     ["Reference"] = "🅡 ",
--     ["Function"] = "🅕 ",
--     ["Method"] = "🅜 ",
--     ["Constructor"] = "🅒 ",
--     ["Module"] = "🅜 ",
--     ["File"] = "🅕 ",
--     ["Folder"] = "🅕 ",
--     ["Keyword"] = "🅚 ",
--     ["Operator"] = "🅞 ",
--     ["Snippet"] = "🅢 ",
--     ["Value"] = "🅥 ",
--     ["Color"] = "🅒 ",
--     ["Event"] = "🅔 ",
--     ["Text"] = "🅣 ",
-- }
-- local cmp_format = function(entry, item)
--         local content = item.abbr
--         -- local kind_symbol = symbols[item.kind]
--         -- item.kind = kind_symbol .. get_ws(MAX_KIND_WIDTH, #kind_symbol)
--
--         if #content > MAX_LABEL_WIDTH then
--                 item.abbr = vim.fn.strcharpart(content, 0, MAX_LABEL_WIDTH) .. ELLIPSIS_CHAR
--         else
--                 item.abbr = content .. get_ws(MAX_LABEL_WIDTH, #content)
--         end
--         item.menu = item.kind
--         item.kind = kind_icons[item.kind] or " "
--         return item
-- end
--
-- -- local cmp_format = function(entry, item)
-- --         local content = item.abbr
-- --         -- local kind_symbol = symbols[item.kind]
-- --         -- item.kind = kind_symbol .. get_ws(MAX_KIND_WIDTH, #kind_symbol)
-- --
-- --         if #content > MAX_LABEL_WIDTH then
-- --                 item.abbr = vim.fn.strcharpart(content, 0, MAX_LABEL_WIDTH) .. ELLIPSIS_CHAR
-- --         else
-- --                 item.abbr = content .. get_ws(MAX_LABEL_WIDTH, #content)
-- --         end
-- --         item.kind = menu_icon[item.kind]
-- --         --item.kind = item.kind[1]
-- --         local strings = vim.split(item.kind, "%s", { trimempty = true })
-- --         item.kind = " " .. (strings[1] or "") .. " "
-- --         item.menu = ({
-- --                 nvim_lsp = 'λ',
-- --                 luasnip = '⋗',
-- --                 buffer = 'Ω',
-- --                 path = '🖫',
-- --                 nvim_lua = 'Π',
-- --         })[entry.source.name]
-- --
-- --         return item
-- -- end
-- local cmp = require('cmp')
-- -- local compare = require('cmp.config.compare')
-- lsp.setup_nvim_cmp({
--         -- entries = { max_item_count = 12 },
--         sources = {
--                 { name = 'nvim_lsp' },
--                 { name = 'path' },
--                 { name = 'fuzzy_buffer' },
--                 { name = "crates" },
--                 { name = 'buffer', keyword_length = 4 },
--                 { name = 'luasnip', keyword_length = 2 },
--         },
--         		mapping = {
-- 		["<C-n>"] = cmp.mapping(
-- 				cmp.mapping.select_next_item(),
-- 				{ "i", "c" }
-- 			),
-- 		["<C-p>"] = cmp.mapping(
-- 				cmp.mapping.select_prev_item(),
-- 				{ "i", "c" }
-- 			),
-- 		['<up>'] = vim.NIL,
-- 	        ['<down>'] = vim.NIL,
-- 	        -- ['<Tab>'] = cmp.mapping(
--                 --                 cmp.mapping.select_next_item(),
--                 --                 {"i", "c"}
-- 	               --  ), 
--                 -- ['<S-Tab>'] = cmp.mapping(
--                 --                 cmp.mapping.select_prev_item(),
--                 --                 {"i", "c"}
--                 --         ),
--                 ['<CR>'] = vim.NIL,
-- 	},
--         formatting = {
--                 -- changing the order of fields so the icon is the first
--                 fields = { 'kind', 'abbr', 'menu' },
--                 format = cmp_format,
--
--                 -- here is where the change happens
--                 -- format = function(entry, item)
--                 --         local menu_icon = {
--                 --                 nvim_lsp = 'λ',
--                 --                 luasnip = '⋗',
--                 --                 buffer = 'Ω',
--                 --                 path = '🖫',
--                 --                 nvim_lua = 'Π',
--                 --         }
--                 --
--                 --         item.menu = menu_icon[entry.source.name]
--                 --         return item
--                 -- end,
--         },
--         -- max_item_count = 12,
--         preselect = 'none',
--         completion = {
--                 completeopt = 'menu,menuone,noinsert,noselect',
--                 -- winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
--                 -- col_offset = -3,
--                 -- side_padding = 0,
--         },
--         window = {
--                 completion = { -- rounded border; thin-style scrollbar
--                         border = 'rounded',
--                         scrollbar = '║',
--                 },
--         },
--         sorting = {
--                 priority_weight = 2,
--                 -- comparators = {
--                 --         require('cmp_fuzzy_buffer.compare'),
--                 --         compare.offset,
--                 --         compare.exact,
--                 --         compare.score,
--                 --         compare.recently_used,
--                 --         compare.kind,
--                 --         compare.sort_text,
--                 --         compare.length,
--                 --         compare.order,
--                 -- }
--         },
--         experimental = {
--                 native_menu = false,
--                 ghost_text = true,
--         },
--
-- })
--
-- don't initialize this language server
-- we will use rust-tools to setup rust_analyzer
lsp.skip_server_setup({ 'rust_analyzer' })

lsp.on_attach(function(_, bufnr)
        -- require("lsp-format").on_attach(client)
        local opts = { buffer = bufnr }
        local bind = vim.keymap.set

        bind('n', '<leader>r', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)

        print('Greetings from on_attach')
end)

lsp.configure('pyright', {
        on_attach = function(_, bufnr)
                -- custom_attach(client, bufnr)
                -- 'Organize imports' keymap for pyright only
                vim.keymap.set("n", "<Leader>ii", "<cmd>PyrightOrganizeImports<CR>", {
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
                                diagnosticMode = "workspace",
                                autoImportCompletions = true,
                        },
                },
        },
})
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
        signs = true,
        update_in_insert = false,
        underline = true,
        severity_sort = false,
        float = true,
})
-- Initialize rust_analyzer with rust-tools
local rust_lsp = lsp.build_options('rust_analyzer', {})

local extension_path = vim.env.HOME .. '/.vscode-oss/extensions/vadimcn.vscode-lldb-1.8.1-universal/'
local codelldb_path = extension_path .. 'adapter/codelldb'
local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'
require('rust-tools').setup({
        server = { rust_lsp,
                on_attach = function(_, bufnr)
                        local rt = require "rust-tools"
                        vim.keymap.set("n", "!", "<cmd>:BaconLoad<CR>:w<CR>:BaconNext<CR>", {buffer = bufnr})
                        -- Hover actions
                        vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
                        -- Code action groups
                        vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
                        -- Rust Runables
                        vim.keymap.set("n", "rr", rt.runnables.runnables, { buffer = bufnr })
                        -- Rust Cargo Open
                        vim.keymap.set("n", "rc", rt.open_cargo_toml.open_cargo_toml, { buffer = bufnr })

                end,
        },
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

-- Format on save
