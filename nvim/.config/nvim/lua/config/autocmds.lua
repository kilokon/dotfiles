local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- General Settings

local general = augroup("General Settings", { clear = true })

autocmd("BufEnter", {
  pattern = { "*.md", "*.txt" },
  callback = function()
    vim.opt_local.spell = true
  end,
  group = general,
  desc = "Enable spell checking on specific filetypes",
})

autocmd("BufEnter", {
  callback = function()
    vim.opt.formatoptions:remove({ "c", "r", "o" })
  end,
  group = general,
  desc = "Disable New Line Comment",
})

-- Go to last loc when opening a buffer
vim.api.nvim_create_autocmd("BufReadPost", {
  group = vim.api.nvim_create_augroup("last_loc", { clear = true }),
  callback = function(event)
    local exclude = { "gitcommit" }
    local buf = event.buf
    if vim.tbl_contains(exclude, vim.bo[buf].filetype) or vim.b[buf].lazyvim_last_loc then
      return
    end
    vim.b[buf].lazyvim_last_loc = true
    local mark = vim.api.nvim_buf_get_mark(buf, '"')
    local lcount = vim.api.nvim_buf_line_count(buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Auto create dir when saving a file, in case some intermediate directory does not exist
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  group = vim.api.nvim_create_augroup("auto_create_dir", { clear = true }),
  callback = function(event)
    if event.match:match("^%w%w+://") then
      return
    end
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
  end,
})
-- -- Use LspAttach autocommand to only map the following keys
-- -- after the language server attaches to the current buffer
-- vim.api.nvim_create_autocmd("LspAttach", {
--   group = vim.api.nvim_create_augroup("UserLspConfig", {}),
--   callback = function(ev)
--     -- Enable completion triggered by <c-x><c-o>
--     vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
--
--     -- Buffer local mappings.
--     -- See `:help vim.lsp.*` for documentation on any of the below functions
--     local opts = { buffer = ev.buf }
--     vim.keymap.set("n", "<leader>gD", vim.lsp.buf.declaration, opts)
--     vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, opts)
--     vim.keymap.set("n", "<leader>gK", vim.lsp.buf.hover, opts)
--     vim.keymap.set("n", "<leader>gi", vim.lsp.buf.implementation, opts)
--     vim.keymap.set("n", "<leader><C-k>", vim.lsp.buf.signature_help, opts)
--     vim.keymap.set("n", "<leader>ga", vim.lsp.buf.add_workspace_folder, opts)
--     vim.keymap.set("n", "<leader>gr", vim.lsp.buf.remove_workspace_folder, opts)
--     vim.keymap.set("n", "<leader>gl", function()
--       print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
--     end, opts)
--     vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, opts)
--     -- vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, opts)
--     vim.keymap.set({ "n", "v" }, "<leader>gc", vim.lsp.buf.code_action, opts)
--     -- vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
--   end,
-- })
--
-- LSP attach: completion and keybindings
-- autocmd("LspAttach", {
--   callback = function(args)
--     local client = vim.lsp.get_client_by_id(args.data.client_id)
--     -- vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
--     local bufopts = { buffer = args.buf }
--     vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
--     vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
--     vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts)
--     vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, bufopts)
--   end,
-- })
--
-- autocmd("FileType", {
--   pattern = "lua",
--   callback = function()
--     vim.lsp.start({
--       name = "lua_ls",
--       cmd = { "lua-language-server" }, -- Assumes it’s in $PATH
--       root_dir = vim.fs.dirname(vim.fs.find({ ".stylua.toml", ".luarc.json", ".git" }, { upward = true })[1]), -- Project root detection
--       settings = {
--         Lua = {
--           diagnostics = {
--             globals = { "vim" }, -- Recognize Neovim’s 'vim' global
--           },
--           workspace = {
--             library = vim.api.nvim_get_runtime_file("", true), -- Include Neovim runtime files
--           },
--         },
--       },
--     })
--   end,
-- })
--
-- -- Start LSP for Typst
-- autocmd("FileType", {
--   pattern = "typst",
--   callback = function()
--     vim.lsp.start({
--       name = "tinymist",
--       cmd = { "tinymist" },
--       root_dir = vim.fs.dirname(vim.fs.find({ ".git", "main.typ" }, { upward = true })[1]),
--     })
--   end,
-- })
-- vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
--   pattern = "*.typ",
--   callback = function()
--     vim.bo.filetype = "typst"
--   end,
-- })
--
-- -- Diagnostics display
-- vim.diagnostic.config({
--   virtual_text = true,
--   signs = true,
--   update_in_insert = false,
-- })
