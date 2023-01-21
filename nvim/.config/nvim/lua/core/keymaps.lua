local function map(mode, lhs, rhs, opts)
        local options = { noremap = true, silent = true }
        if opts then
                options = vim.tbl_extend("force", options, opts)
        end
        vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local opts = { noremap = true, silent = true }


-- Telescope
--map("n", "<leader>ff", ":Telescope find_files<CR>")
--map("n", "<leader>fg", ":Telescope live_grep<CR>")
--map("n", "<leader>fh", ":Telescope help_tags<CR>")
--map("n", "<leader>fb", ":Telescope buffers<CR>")

map("n", "<leader>ff",
        "<cmd>lua require('telescope.builtin').find_files(require('telescope.themes').get_dropdown({ previewer = false }))<cr>"
        , opts)
map("n", "<leader>fg", "<cmd>lua require('telescope.builtin').live_grep()<cr>", opts)
map("n", "<leader>bb", "<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown())<cr>",
        opts)
map("n", "<leader>fb", "<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>", opts)


-- Rust-Tools
map("n", "rr", "<cmd>RustHoverActions<CR><cmd>RustHoverActions<CR>")
map("n", "rc", "<cmd>RustOpenCargo<CR>")
map("n", "rd", "<cmd>RustOpenExternalDocs<CR>")
