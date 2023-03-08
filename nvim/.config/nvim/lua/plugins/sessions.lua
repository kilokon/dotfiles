local resession = require('resession')
resession.setup({
        -- Options for automatically saving sessions on a timer
        autosave = {
                enabled = false,
                -- How often to save (in seconds)
                interval = 60,
                -- Notify when autosaved
                notify = true,
        },
        -- Save and restore these options
        options = {
                "binary",
                "bufhidden",
                "buflisted",
                "cmdheight",
                "diff",
                "filetype",
                "modifiable",
                "previewwindow",
                "readonly",
                "scrollbind",
                "winfixheight",
                "winfixwidth",
        },
        -- Custom logic for determining if the buffer should be included
        buf_filter = require("resession").default_buf_filter,
        -- Custom logic for determining if a buffer should be included in a tab-scoped session
        tab_buf_filter = function(tabpage, bufnr)
                return true
        end,
        -- The name of the directory to store sessions in
        dir = "session",
        -- Show more detail about the sessions when selecting one to load.
        -- Disable if it causes lag.
        load_detail = true,
        -- Configuration for extensions
        extensions = {
                quickfix = {},
        },
})

-- Automatically save a session when you exit Neovim
vim.api.nvim_create_autocmd("VimLeavePre", {
        callback = function()
                -- Always save a special session named "last"
                resession.save("last")
        end,
})

local function get_session_name()
        local name = vim.fn.getcwd()
        local branch = vim.fn.system("git branch --show-current")
        if vim.v.shell_error == 0 then
                return name .. branch
        else
                return name
        end
end

vim.api.nvim_create_autocmd("VimEnter", {
        callback = function()
                -- Only load the session if nvim was started with no args
                if vim.fn.argc(-1) == 0 then
                        resession.load(get_session_name(), { dir = "dirsession", silence_errors = true })
                end
        end,
})

vim.api.nvim_create_autocmd("VimLeavePre", {
        callback = function()
                resession.save(get_session_name(), { dir = "dirsession", notify = false })
        end,
})


-- Resession does NOTHING automagically, so we have to set up some keymaps

local wk = require("which-key")
-- local opts_n = require("drystuff.noremaps").map_options("n")
local mappings = {
        s = {
                name = "Ression",
                s = { "<cmd>lua resession.save", "Save Session" },
                l = { "<cmd>lua resession.load", "Load Session" },
                d = { "<cmd>lua resession.delete", "Delete Session" },
        }
}
wk.register(mappings, { prefix = "<leader>" })
