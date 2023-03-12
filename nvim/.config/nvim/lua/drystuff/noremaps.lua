local M = {}
M.keyremap = function(mode, lhs, rhs, opts)
        -- local function map(mode, lhs, rhs, opts)
        local options = { noremap = true, silent = true }
        if opts then
                options = vim.tbl_extend("force", options, opts)
        end
        return vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

M.map_options = function(tag)
        local opts_tags = { "n", "v" }

        for _, value in ipairs(opts_tags) do
                if value == tag then
                        return {
                                {
                                        mode = tag, --
                                        -- prefix: use "<leader>f" for example for mapping everything related to finding files
                                        -- the prefix is prepended to every mapping part of `mappings`
                                        prefix = "<leader>",
                                        buffer = nil,   -- Global mappings. Specify a buffer number for buffer local mappings
                                        silent = true,  -- use `silent` when creating keymaps
                                        noremap = true, -- use `noremap` when creating keymaps
                                        nowait = false, -- use `nowait` when creating keymaps
                                }
                        }
                end
        end
end


return M
