require("nap").setup({
        next_prefix = "<c-n>",
        prev_prefix = "<c-p>",
        next_repeat = "<c-n>",
        prev_repeat = "<c-p>",
        operators = {
                ["a"] = {
                        next = { command = "tabnext", desc = "Next tab", },
                        prev = { command = "tabprevious", desc = "Prev tab", },
                },
                ["A"] = {
                        next = { command = "tablast", desc = "Last tab", },
                        prev = { command = "tabfirst", desc = "First tab", },
                },
                ["b"] = {
                        next = { command = "bnext", desc = "Next buffer", },
                        prev = { command = "bprevious", desc = "Prev buffer", },
                },
                ["B"] = {
                        next = { command = "blast", desc = "Last buffer", },
                        prev = { command = "bfirst", desc = "First buffer", },
                },
                ["d"] = {
                        next = { command = vim.diagnostic.goto_next, desc = "Next diagnostic", },
                        prev = { command = vim.diagnostic.goto_prev, desc = "Prev diagnostic", },
                        mode = { "n", "v", "o" }
                },
                ["e"] = {
                        next = { command = "normal! g;", desc = "Older edit (change-list) item", },
                        prev = { command = "normal! g,", desc = "Newer edit (change-list) item", }
                },
                -- ["f"] = {
                --     next = { command = M.next_file, desc = "Next file", },
                --     prev = { command = M.prev_file, desc = "Prev file", },
                -- },
                -- ["F"] = {
                --     next = { command = M.last_file, desc = "Last file", },
                --     prev = { command = M.first_file, desc = "First file", },
                -- },
                -- ["j"] = {
                --     next = { command = M.next_jump_list, desc = "Older jump-list item", },
                --     prev = { command = M.prev_jump_list, desc = "Newer jump-list item" },
                -- },
                ["l"] = {
                        next = { command = "lnext", desc = "Next loclist item", },
                        prev = { command = "lprevious", desc = "Prev loclist item" },
                },
                ["L"] = {
                        next = { command = "llast", desc = "Last loclist item", },
                        prev = { command = "lfirst", desc = "First loclist item" },
                },
                ["<C-l>"] = {
                        next = { command = "lnfile", desc = "Next loclist item in different file", },
                        prev = { command = "lpfile", desc = "Prev loclist item in different file" },
                },
                ["<M-l>"] = {
                        next = { command = "lnewer", desc = "Next loclist list", },
                        prev = { command = "lolder", desc = "Prev loclist list" },
                },
                ["q"] = {
                        next = { command = "cnext", desc = "Next quickfix item", },
                        prev = { command = "cprevious", desc = "Prev quickfix item" },
                },
                ["Q"] = {
                        next = { command = "clast", desc = "Last quickfix item", },
                        prev = { command = "cfirst", desc = "First quickfix item" },
                },
                ["<C-q>"] = {
                        next = { command = "cnfile", desc = "Next quickfix item in different file", },
                        prev = { command = "cpfile", desc = "Prev quickfix item in different file" },
                },
                ["<M-q>"] = {
                        next = { command = "cnewer", desc = "Next quickfix list", },
                        prev = { command = "colder", desc = "Prev quickfix list" },
                },
                ["s"] = {
                        next = { command = "normal! ]s", desc = "Next spell error", },
                        prev = { command = "normal! [s", desc = "Prev spell error", },
                },
                ["t"] = {
                        next = { command = "tnext", desc = "Next tag", },
                        prev = { command = "tprevious", desc = "Prev tag" },
                },
                ["T"] = {
                        next = { command = "tlast", desc = "Last tag", },
                        prev = { command = "tfirst", desc = "First tag" },
                },
                ["<C-t>"] = {
                        next = { command = "ptnext", desc = "Next tag in previous window", },
                        prev = { command = "ptprevious", desc = "Prev tag in previous window" },
                },
                ["z"] = {
                        next = { command = "normal! zj", desc = "Next fold", },
                        prev = { command = "normal! zk", desc = "Prev fold", },
                        mode = { "n", "v", "o" },
                },
                ["'"] = {
                        next = { command = "normal! ]`", desc = "Next lowercase mark", },
                        prev = { command = "normal! [`", desc = "Prev lowercase mark" },
                },
        }
})
