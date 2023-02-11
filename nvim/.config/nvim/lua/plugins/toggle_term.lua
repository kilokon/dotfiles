local status_ok, toggleterm = pcall(require, "toggleterm")
if not status_ok then
        return
end

toggleterm.setup({
        size = 20,
        open_mapping = [[<c-`>]],
        shade_filetypes = {},
        shade_terminals = false,
        start_in_insert = true,
        persist_size = true,
        direction = "horizontal",
        hide_numbers = true, -- hide the number column in toggleterm buffers
        shell = vim.o.shell
})

function _G.set_terminal_keymaps()
        local opts = { noremap = true }
        vim.api.nvim_buf_set_keymap(0, "t", "<esc>", [[<C-\><C-n>]], opts)
        vim.api.nvim_buf_set_keymap(0, "t", "jk", [[<C-\><C-n>]], opts)
        vim.api.nvim_buf_set_keymap(0, "t", "<C-h>", [[<C-\><C-n><C-W>h]], opts)
        vim.api.nvim_buf_set_keymap(0, "t", "<C-j>", [[<C-\><C-n><C-W>j]], opts)
        vim.api.nvim_buf_set_keymap(0, "t", "<C-k>", [[<C-\><C-n><C-W>k]], opts)
        vim.api.nvim_buf_set_keymap(0, "t", "<C-l>", [[<C-\><C-n><C-W>l]], opts)
end

vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")

-- Terminal:new {
--   cmd = string -- command to execute when creating the terminal e.g. 'top'
--   direction = string -- the layout for the terminal, same as the main config options
--   dir = string -- the directory for the terminal
--   close_on_exit = bool -- close the terminal window when the process exits
--   highlights = table -- a table with highlights
--   env = table -- key:value table with environmental variables passed to jobstart()
--   clear_env = bool -- use only environmental variables from `env`, passed to jobstart()
--   on_open = fun(t: Terminal) -- function to run when the terminal opens
--   on_close = fun(t: Terminal) -- function to run when the terminal closes
--   auto_scroll = boolean -- automatically scroll to the bottom on terminal output
--   -- callbacks for processing the output
--   on_stdout = fun(t: Terminal, job: number, data: string[], name: string) -- callback for processing output on stdout
--   on_stderr = fun(t: Terminal, job: number, data: string[], name: string) -- callback for processing output on stderr
--   on_exit = fun(t: Terminal, job: number, exit_code: number, name: string) -- function to run when terminal process exits
-- }
-- local python = Terminal:new({ cmd = "python", hidden = true })
--
-- function _PYTHON_TOGGLE()
-- 	python:toggle()
-- end
