-- stevearc/overseer.nvim
require('overseer').setup({
        -- Tasks are disposed 5 minutes after running to free resources.
        -- If you need to close a task inmediatelly:
        -- press ENTER in the menu you see after compiling on the task you want to close.
        task_list = {
                direction = "bottom",
                min_height = 25,
                max_height = 25,
                default_detail = 1,
                bindings = {
                        ["q"] = function() vim.cmd("OverseerClose") end,
                },
        },
})
