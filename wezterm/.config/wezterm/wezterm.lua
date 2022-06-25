local wezterm = require("wezterm")

local set_environment_variables = {}

return {
  default_prog = { "/usr/bin/fish" },
  set_environment_variables = set_environment_variables,
  -- font = wezterm.font("JetBrains Mono"),
  font = wezterm.font("RobotoMono Nerd Font"),
  font_size = 11.2,
  color_scheme = "tokyonight-storm",
  hide_tab_bar_if_only_one_tab = true,
  window_background_opacity = 0.7,
  text_background_opacity = 0.6,

  leader = { key = "a", mods = "CTRL", timeout_milliseconds = 3000 },
  keys = {
    {
      key = "r",
      mods = "LEADER",
      action = wezterm.action({
        ActivateKeyTable = {
          name = "resize_pane",
          one_shot = false,
          replace_current = true,
        },
      }),
    },
    {
      key = "a",
      mods = "LEADER",
      action = wezterm.action({
        ActivateKeyTable = {
          name = "activate_pane",
          timeout_milliseconds = 1000,
          one_shot = false,
          replace_current = false,
        },
      }),
    },
    {
      key = "|",
      mods = "LEADER|SHIFT",
      action = wezterm.action({
        SplitHorizontal = { domain = "CurrentPaneDomain" },
      }),
    },
    {
      key = "-",
      mods = "LEADER",
      action = wezterm.action({
        SplitVertical = { domain = "CurrentPaneDomain" },
      }),
    },
    {
      key = "b",
      mods = "CTRL",
      action = wezterm.action({ RotatePanes = "CounterClockwise" }),
    },
    {
      key = "n",
      mods = "CTRL",
      action = wezterm.action({
        RotatePanes = "Clockwise",
      }),
    },
    { key = "0", mods = "SHIFT|CTRL", action = "SpawnWindow" },
    {
      key = "9",
      mods = "ALT",
      action = wezterm.action({
        ShowLauncherArgs = { flags = "FUZZY|WORKSPACES" },
      }),
    },
    {
      key = "o",
      mods = "ALT",
      action = wezterm.action({
        SwitchWorkspaceRelative = 1,
      }),
    },
    {
      key = "p",
      mods = "CTRL",
      action = wezterm.action({ SwitchWorkspaceRelative = -1 }),
    },
  },
  key_tables = {
    -- Defines the keys that are active in our resize-pane mode.
    -- Since we're likely to want to make multiple adjustments,
    -- we made the activation one_shot=false. We therefore need
    -- to define a key assignment for getting out of this mode.
    -- 'resize_pane' here corresponds to the name="resize_pane" in
    -- the key assignments above.
    resize_pane = {
      {
        key = "LeftArrow",
        action = wezterm.action({ AdjustPaneSize = { "Left", 1 } }),
      },
      {
        key = "h",
        action = wezterm.action({ AdjustPaneSize = { "Left", 1 } }),
      },

      {
        key = "RightArrow",
        action = wezterm.action({ AdjustPaneSize = { "Right", 1 } }),
      },
      {
        key = "l",
        action = wezterm.action({ AdjustPaneSize = { "Right", 1 } }),
      },

      {
        key = "UpArrow",
        action = wezterm.action({ AdjustPaneSize = { "Up", 1 } }),
      },
      { key = "k", action = wezterm.action({ AdjustPaneSize = { "Up", 1 } }) },

      {
        key = "DownArrow",
        action = wezterm.action({ AdjustPaneSize = { "Down", 1 } }),
      },
      {
        key = "j",
        action = wezterm.action({ AdjustPaneSize = { "Down", 1 } }),
      },

      -- Cancel the mode by pressing escape
      { key = "Escape", action = "PopKeyTable" },
    },
    activate_pane = {
      {
        key = "LeftArrow",
        action = wezterm.action({
          ActivatePaneDirection = "Left",
        }),
      },
      {
        key = "h",
        action = wezterm.action({ ActivatePaneDirection = "Left" }),
      },

      {
        key = "RightArrow",
        action = wezterm.action({ ActivatePaneDirection = "Right" }),
      },
      {
        key = "l",
        action = wezterm.action({ ActivatePaneDirection = "Right" }),
      },

      {
        key = "UpArrow",
        action = wezterm.action({ ActivatePaneDirection = "Up" }),
      },
      { key = "k", action = wezterm.action({ ActivatePaneDirection = "Up" }) },

      {
        key = "DownArrow",
        action = wezterm.action({
          ActivatePaneDirection = "Down",
        }),
      },
      {
        key = "j",
        action = wezterm.action({ ActivatePaneDirection = "Down" }),
      },
    },
  },
}
