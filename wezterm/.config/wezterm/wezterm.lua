-- WezTerm configuration
---------------------------------------------------------------
local wezKeys = require("keymap")
local wezterm = require("wezterm")

local config = wezterm.config_builder()
config = {
  default_prog = { "/usr/bin/nu", "-l" },
  default_cursor_style = "SteadyBar",
  font = wezterm.font_with_fallback({
    {
      family = "JetBrains Mono",
      harfbuzz_features = { "calt=1", "clig=0", "liga=0" },
    },
    {
      family = "Iosevka",
      harfbuzz_features = { "calt=1", "clig=1", "liga=1" },
    },
  }),

  font_size = 11,
  color_scheme = "tokyonight-storm",
  hide_tab_bar_if_only_one_tab = true,
  window_background_opacity = 0.7,
  text_background_opacity = 0.5,
  disable_default_key_bindings = true,
  enable_wayland = false,
  use_fancy_tab_bar = false,
  tab_bar_at_bottom = true,
  tab_max_width = 30,
  window_decorations = "RESIZE",
  inactive_pane_hsb = { saturation = 0.7, brightness = 0.7 },
  leader = { key = "a", mods = "CTRL", timeout_milliseconds = 1000 },
  keys = wezKeys.keys(),
  -- config.key_tables = wezKeys.key_tables()
  window_close_confirmation = "NeverPrompt",
  window_frame = {
    font = wezterm.font({ family = "JetBrains Mono", weight = "Bold" }),
    font_size = 11.0,
    border_left_width = "0.0cell",
    border_right_width = "0.0cell",
    border_bottom_height = "0.10cell",
    border_bottom_color = "#1a1b26",
    border_top_height = "0.0cell",
  },
  underline_thickness = 1,
  underline_position = -2.0,
  launch_menu = {
    {
      label = "Launch Nu",
      args = { "nu", "-l" },
      cwd = "~/dotfiles",
    },
    {
      label = "Launch Bash",
      args = { "bash", "-l" },
      cwd = "~/dotfiles",
    },
  },
}

local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")
resurrect.state_manager.periodic_save({
  interval_seconds = 15 * 60,
  save_workspaces = true,
  save_windows = true,
  save_tabs = true,
})

wezterm.on("resurrect.error", function(err)
  wezterm.log_error("ERROR!")
  -- wezterm.gui.gui_windows()[1]:toast_notification("resurrect", err, nil, 3000)
end)

--PLugins
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
workspace_switcher.apply_to_config(config)

local smart_splits = wezterm.plugin.require("https://github.com/mrjones2014/smart-splits.nvim")
smart_splits.apply_to_config(config, {
  direction_keys = {
    move = { "h", "j", "k", "l" },
    resize = { "LeftArrow", "DownArrow", "UpArrow", "RightArrow" },
  },
  -- modifier keys to combine with direction_keys
  modifiers = {
    move = "CTRL", -- modifier to use for pane movement, e.g. CTRL+h to move left
    resize = "META", -- modifier to use for pane resize, e.g. META+h to resize to the left
  },
  -- log level to use: info, warn, error
  log_level = "info",
})

return config
