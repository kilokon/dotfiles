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
  }
})
