vim.api.nvim_create_autocmd("FileType", {
    pattern = "lua",
    callback = function()
        vim.lsp.start({
            name = "lua-language-server",
            cmd = { "lua-language-server" },
            root_dir = vim.fs.dirname(vim.fs.find({ ".stylua.toml" }, { upward = true })[1]),
        })
    end,
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = "python",
    callback = function()
        vim.lsp.start({
            name = "pyright",
            cmd = { "pyright-langserver", "--stdio" },
            root_dir = vim.fs.dirname(vim.fs.find({ "pyproject.toml", "setup.py", ".git" }, { upward = true })[1]),
        })
    end,
})

-- Start LSP for Rust
vim.api.nvim_create_autocmd("FileType", {
    pattern = "rust",
    callback = function()
        vim.lsp.start({
            name = "rust-analyzer",
            cmd = { "rust-analyzer" },
            root_dir = vim.fs.dirname(vim.fs.find({ "Cargo.toml" }, { upward = true })[1]),
            settings = {
                ["rust-analyzer"] = {
                    inlayHints = {
                        enable = true,
                    },
                    diagnostics = {
                        enable = true,
                    },
                },
            },
        })
    end,
})

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local bufopts = { buffer = args.buf }
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts) -- Go to definition
        vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts) -- Show hover info
        vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts) -- Rename symbol
        vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, bufopts) -- Code actions
    end,
})
