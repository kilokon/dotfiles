-- Things to read before configuring Neovim
-- https://gist.github.com/dtr2300/2f867c2b6c051e946ef23f92bd9d1180 - Events

require("core.keymap")
require("core.filetypes")

local rocks_config = {
    rocks_path = vim.env.HOME .. "/.local/share/nvim/rocks",
}

vim.g.rocks_nvim = rocks_config

local luarocks_path = {
    vim.fs.joinpath(rocks_config.rocks_path, "share", "lua", "5.1", "?.lua"),
    vim.fs.joinpath(rocks_config.rocks_path, "share", "lua", "5.1", "?", "init.lua"),
}
package.path = package.path .. ";" .. table.concat(luarocks_path, ";")

local luarocks_cpath = {
    vim.fs.joinpath(rocks_config.rocks_path, "lib", "lua", "5.1", "?.so"),
    vim.fs.joinpath(rocks_config.rocks_path, "lib64", "lua", "5.1", "?.so"),
}
package.cpath = package.cpath .. ";" .. table.concat(luarocks_cpath, ";")

vim.opt.runtimepath:append(vim.fs.joinpath(rocks_config.rocks_path, "lib", "luarocks", "rocks-5.1", "*", "*"))

require("core.lsp_setup")

-- vim.lsp.config["luals"] = {
--     -- Command and arguments to start the server.
--     cmd = { "lua-language-server" },
--     filetypes = { "lua" },
--     root_markers = { ".stylua.toml", ".luarc.json", ".luarc.jsonc" },
--     settings = {
--         Lua = {
--             runtime = {
--                 version = "LuaJIT",
--             },
--         },
--     },
-- }
--
-- vim.lsp.enable("luals")
--
-- vim.lsp.config("*", {
--     capabilities = {
--         textDocument = {
--             semanticTokens = {
--                 multilineTokenSupport = true,
--             },
--         },
--     },
-- })
-- vim.lsp.config("clangd", require("lspconfig.configs.clangd").default_config)

vim.api.nvim_create_autocmd({ "TermOpen" }, {
    command = "startinsert",
})
vim.keymap.set("t", "<esc>", "<c-\\><c-n>")
