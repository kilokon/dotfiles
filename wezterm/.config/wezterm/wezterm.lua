-- WezTerm configuration
---------------------------------------------------------------
local wezKeys = require("keymap")
local wezterm = require("wezterm")

local config = wezterm.config_builder()
config = {
  default_prog = { "/usr/bin/nu", "-l" },
  default_cursor_style = "SteadyBar",
  font = wezterm.font_with_fallback({
    -- {
    --   family = "FiraCode Nerd Font",
    --   weight = "Regular",
    --   harfbuzz_features = {
    --     -- "cv01", ---styles: a
    --     -- "cv02", ---styles: g
    --     "cv06", ---styles: i (03..06)
    --     -- "cv09", ---styles: l (07..10)
    --     "cv12", ---styles: 0 (11..13, zero)
    --     "cv14", ---styles: 3
    --     "cv16", ---styles: * (15..16)
    --     -- "cv17", ---styles: ~
    --     -- "cv18", ---styles: %
    --     -- "cv19", ---styles: <= (19..20)
    --     -- "cv21", ---styles: =< (21..22)
    --     -- "cv23", ---styles: >=
    --     -- "cv24", ---styles: /=
    --     "cv25", ---styles: .-
    --     "cv26", ---styles: :-
    --     -- "cv27", ---styles: []
    --     "cv28", ---styles: {. .}
    --     "cv29", ---styles: { }
    --     -- "cv30", ---styles: |
    --     "cv31", ---styles: ()
    --     "cv32", ---styles: .=
    --     -- "ss01", ---styles: r
    --     -- "ss02", ---styles: <= >=
    --     "ss03", ---styles: &
    --     "ss04", ---styles: $
    --     "ss05", ---styles: @
    --     -- "ss06", ---styles: \\
    --     "ss07", ---styles: =~ !~
    --     -- "ss08", ---styles: == === != !==
    --     "ss09", ---styles: >>= <<= ||= |=
    --     -- "ss10", ---styles: Fl Tl fi fj fl ft
    --     -- "onum", ---styles: 1234567890
    --   },
    -- },
    -- { family = "Noto Color Emoji" },
    {
      family = "JetBrains Mono",
      weight = "Regular",
      harfbuzz_features = { "calt=1", "clig=0", "liga=0" },
    },
    -- {
    --   family = "Iosevka",
    --   harfbuzz_features = { "calt=1", "clig=1", "liga=1" },
    -- },
  }),

  font_size = 10.5,
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
  front_end = "WebGpu",
  webgpu_power_preference = "HighPerformance",
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
-- loads the state whenever I create a new workspace
wezterm.on("smart_workspace_switcher.workspace_switcher.created", function(window, path, label)
  local workspace_state = resurrect.workspace_state

  workspace_state.restore_workspace(resurrect.state_manager.load_state(label, "workspace"), {
    window = window,
    relative = true,
    restore_text = true,
    on_pane_restore = resurrect.tab_state.default_on_pane_restore,
  })
end)
wezterm.on("smart_workspace_switcher.workspace_switcher.selected", function(window, path, label)
  local workspace_state = resurrect.workspace_state
  resurrect.state_manager.save_state(workspace_state.get_workspace_state())
end)

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
