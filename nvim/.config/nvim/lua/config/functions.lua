vim.api.nvim_create_user_command("FlaskRun", function()
  local flask_env = os.getenv("FLASK_ENV")
  local current_file = vim.fn.expand("%:p") -- Get full path of the current file

  -- Check if FLASK_ENV is set
  if not flask_env then
    vim.api.nvim_err_writeln("FLASK_ENV not set in the environment!")
    return
  end

  -- Check if the current file is a Python file
  if not current_file:match("%.py$") then
    vim.api.nvim_err_writeln("Current file is not a Python file!")
    return
  end

  -- Run Flask in a WezTerm split
  vim.cmd("silent! !wezterm cli split-pane --bottom --percent 20 -- flask run --debug")
end, {})
