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

vim.opt.runtimepath:append(
  vim.fs.joinpath(rocks_config.rocks_path, "lib", "luarocks", "rocks-5.1", "*", "*")
)

--
-- do
--   -- Specifies where to install/use rocks.nvim
--   local install_location = vim.fs.joinpath(vim.fn.stdpath("data") --[[@as string]], "rocks")
--
--   -- Set up configuration options related to rocks.nvim (recommended to leave as default)
--   local rocks_config = {
--     rocks_path = vim.fs.normalize(install_location),
--   }
--
--   vim.g.rocks_nvim = rocks_config
--
--   -- Configure the package path (so that plugin code can be found)
--   local luarocks_path = {
--     vim.fs.joinpath(rocks_config.rocks_path, "share", "lua", "5.1", "?.lua"),
--     vim.fs.joinpath(rocks_config.rocks_path, "share", "lua", "5.1", "?", "init.lua"),
--   }
--   package.path = package.path .. ";" .. table.concat(luarocks_path, ";")
--
--   -- Configure the C path (so that e.g. tree-sitter parsers can be found)
--   local luarocks_cpath = {
--     vim.fs.joinpath(rocks_config.rocks_path, "lib", "lua", "5.1", "?.so"),
--     vim.fs.joinpath(rocks_config.rocks_path, "lib64", "lua", "5.1", "?.so"),
--   }
--   package.cpath = package.cpath .. ";" .. table.concat(luarocks_cpath, ";")
--
--   -- Add rocks.nvim to the runtimepath
--   vim.opt.runtimepath:append(
--     vim.fs.joinpath(rocks_config.rocks_path, "lib", "luarocks", "rocks-5.1", "rocks.nvim", "*")
--   )
-- end

-- vim.api.nvim_create_autocmd({ "BufReadPre", "BufNewFile" }, {
--   -- vim.api.nvim_create_autocmd({ "InsertLeave", "BufReadPre" }, {
--   callback = function()
--     vim.cmd("packadd gitsigns")
--     require("gitsigns").setup()
--     return true
--   end,
-- })

vim.api.nvim_create_autocmd({ "TermOpen" }, {
  command = "startinsert",
})
vim.keymap.set("t", "<esc>", "<c-\\><c-n>")
