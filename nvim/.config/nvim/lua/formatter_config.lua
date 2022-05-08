local status_ok, fmtr = pcall(require, "formatter")
if not status_ok then
  return
end

fmtr.setup({
        filetype = {
                rust = {
                        -- Rustfmt
                        function()
                                return {
                                        exe = "rustfmt",
                                        args = {"--emit=stdout", "--edition=2021"},
                                        stdin = true
                                }
                        end
                },
                lua = {
                        function()
                                return {
                                        exe = "stylua",
                                        args = {
                                                "--config-path "
                                                        --.. os.getenv("XDG_CONFIG_HOME")
                                                        .. os.getenv("HOME")
                                                        .. "/stylua/stylua.toml",
                                                "-",
                                        },
                                        stdin = true,
                                }
                        end,
                },
        }
})

-- Format on save
vim.api.nvim_exec([[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.js,*.rs,*.lua FormatWrite
augroup END
]], true)
