local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end



--require("logger_config")
require("editor_settings")
require("plugins")
--require("lsp_config")
--require("cmp_config")
require("treesitter")
require("color_scheme_config")
--require("autopairs_config")
require("comment")
require("keymaps")
require("terminal_config")
require("lualine_config")
require("nvim_tree_config")
--require("rust-tools-config")
require("formatter_config")
--require("telescope_config")
require("which_key_config")
