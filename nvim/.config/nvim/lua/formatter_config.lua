local status_ok, fmtr = pcall(require, "formatter")
if not status_ok then
  return
end

-- Utilities for creating configurations
local util = require("formatter.util")

fmtr.setup({
  filetype = {
    rust = {
      -- Rustfmt
      function()
        return {
          exe = "rustfmt",
          args = { "--emit=stdout", "--edition=2021" },
          stdin = true,
        }
      end,
    },
    lua = {
      require("formatter.filetypes.lua").stylua,
      function()
        return {
          exe = "stylua",
          args = {
            "--search-parent-directories",
            "--stdin-filepath",
            util.escape_path(util.get_current_buffer_file_path()),
            "--",
            "-",
          },
          --          args = {
          --            "--config-path "            --.. os.getenv("XDG_CONFIG_HOME")
          -- .. os.getenv(
          --              "HOME"
          --            ) .. "/stylua/stylua.toml",
          --            "-",
          --         },
          stdin = true,
        }
      end,
    },
  },
})

-- Format on save
vim.api.nvim_exec(
  [[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.js,*.rs,*.lua FormatWrite
augroup END
]],
  true
)
