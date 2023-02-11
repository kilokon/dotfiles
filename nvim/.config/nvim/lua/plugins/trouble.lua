require("trouble").setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
}

vim.keymap.set("n", "<f8>", "<cmd>TroubleToggle<cr>",
        { silent = true, noremap = true }
)

vim.keymap.set("n", "<f9>", "<cmd>TroubleToggle quickfix<cr>",
        { silent = true, noremap = true }
)
