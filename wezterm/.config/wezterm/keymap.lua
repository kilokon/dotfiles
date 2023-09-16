local wezterm = require("wezterm")
local act = wezterm.action
local M = {}
M.keys = function()
  return {
    { key = "C", mods = "CTRL",        action = act.CopyTo("ClipboardAndPrimarySelection") },
    { key = "V", mods = "CTRL",        action = act.PasteFrom("Clipboard") },
    { key = "V", mods = "CTRL",        action = act.PasteFrom("PrimarySelection") },
    { key = "k", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Up" }) },
    { key = "j", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Down" }) },
    { key = "h", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Left" }) },
    { key = "l", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Right" }) },
    { key = "l", mods = "LEADER",      action = act.ShowLauncher },             -- },
    { key = "t", mods = "LEADER",      action = act.SpawnCommandInNewTab({}) },
    { key = 'L', mods = 'CTRL',        action = wezterm.action.ShowDebugOverlay },
    {
      key = "|",
      mods = "ALT|SHIFT|CTRL",
      action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
    },
    {
      key = "-",
      mods = "LEADER",
      action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
    },
    -- {
    --         key = "%",
    --         mods = "CTRL|SHIFT|ALT",
    --         action = act.SplitPane({
    --                 direction = "Left",
    --                 -- command = { args = { "top" } },
    --                 size = { Percent = 50 },
    --         }),
    -- },

    -- Create a new workspace with a random name and switch to it
    { key = "i", mods = "LEADER", action = act.SwitchToWorkspace },
    -- Show the launcher in fuzzy selection mode and have it list all workspaces
    -- and allow activating one.
    { key = "/", mods = "LEADER", action = act.ShowLauncherArgs({ flags = "FUZZY|TABS" }) },
    {
      key = "r",
      mods = "LEADER",
      action = act({
        ActivateKeyTable = { name = "resize_pane", one_shot = false, replace_current = true },
      }),
    },
    {
      key = "a",
      mods = "LEADER",
      action = act({
        ActivateKeyTable = {
          name = "activate_pane",
          timeout_milliseconds = 1000,
          one_shot = true,
          replace_current = false,
        },
      }),
    },
    -- {
    --         key = "|",
    --         mods = "LEADER|SHIFT",
    --         action = wezterm.action({
    --                 SplitHorizontal = { domain = "CurrentPaneDomain" },
    --         }),
    -- },
    -- {
    --         key = "-",
    --         mods = "LEADER",
    --         action = wezterm.action({
    --                 SplitVertical = { domain = "CurrentPaneDomain" },
    --         }),
    -- },
    -- {
    --         key = "b",
    --         mods = "LEADER",
    --         action = wezterm.action({ RotatePanes = "CounterClockwise" }),
    -- },
    {
      key = "P",
      mods = "CTRL|ALT",
      action = act({
        RotatePanes = "Clockwise",
      }),
    },
    { key = ")", mods = "CTRL",     action = "SpawnWindow" },
    {
      key = "9",
      mods = "ALT",
      action = act({
        ShowLauncherArgs = { flags = "FUZZY|WORKSPACES" },
      }),
    },
    {
      key = "Tab",
      mods = "ALT",
      action = act({
        SwitchWorkspaceRelative = 1,
      }),
    },
    -- {
    --         key = "P",
    --         mods = "CTRL",
    --         action = wezterm.action({ SwitchWorkspaceRelative = -1 }),
    -- },

    {
      key = "a",
      mods = "LEADER",
      action = act({
        ActivateKeyTable = {
          name = "activate_pane",
          timeout_milliseconds = 1000,
          one_shot = true,
          replace_current = false,
        },
      }),
    },
    { key = "{", mods = "LEADER",   action = act.ActivateTabRelative(-1) },
    { key = "}", mods = "LEADER",   action = act.ActivateTabRelative(1) },
    { key = "1", mods = "CTRL|ALT", action = act.ActivateTab(0) },
    { key = "2", mods = "CTRL|ALT", action = act.ActivateTab(1) },
    { key = "3", mods = "CTRL|ALT", action = act.ActivateTab(2) },
    { key = "4", mods = "CTRL|ALT", action = act.ActivateTab(3) },
    { key = "5", mods = "CTRL|ALT", action = act.ActivateTab(4) },
    { key = "6", mods = "CTRL|ALT", action = act.ActivateTab(5) },
    { key = "7", mods = "CTRL|ALT", action = act.ActivateTab(6) },
    { key = "8", mods = "CTRL|ALT", action = act.ActivateTab(7) },
  }
end
M.key_tables = function()
  return {
    -- Defines the keys that are active in our resize-pane mode.
    -- Since we're likely to want to make multiple adjustments,
    -- we made the activation one_shot=false. We therefore need
    -- to define a key assignment for getting out of this mode.
    -- 'resize_pane' here corresponds to the name="resize_pane" in
    -- the key assignments above.
    resize_pane = {
      {
        key = "LeftArrow",
        action = act({ AdjustPaneSize = { "Left", 1 } }),
      },
      {
        key = "h",
        action = act({ AdjustPaneSize = { "Left", 1 } }),
      },

      {
        key = "RightArrow",
        action = act({ AdjustPaneSize = { "Right", 1 } }),
      },
      {
        key = "l",
        action = act({ AdjustPaneSize = { "Right", 1 } }),
      },

      {
        key = "UpArrow",
        action = act({ AdjustPaneSize = { "Up", 1 } }),
      },
      { key = "k",      action = act({ AdjustPaneSize = { "Up", 1 } }) },

      {
        key = "DownArrow",
        action = act({ AdjustPaneSize = { "Down", 1 } }),
      },
      {
        key = "j",
        action = act({ AdjustPaneSize = { "Down", 1 } }),
      },

      -- Cancel the mode by pressing escape
      { key = "Escape", action = "PopKeyTable" },
    },
    activate_pane = {
      {
        key = "LeftArrow",
        action = act({
          ActivatePaneDirection = "Left",
        }),
      },
      {
        key = "h",
        action = act({ ActivatePaneDirection = "Left" }),
      },

      {
        key = "RightArrow",
        action = act({ ActivatePaneDirection = "Right" }),
      },
      {
        key = "l",
        action = act({ ActivatePaneDirection = "Right" }),
      },

      {
        key = "UpArrow",
        action = act({ ActivatePaneDirection = "Up" }),
      },
      { key = "k", action = act({ ActivatePaneDirection = "Up" }) },

      {
        key = "DownArrow",
        action = act({
          ActivatePaneDirection = "Down",
        }),
      },
      {
        key = "j",
        action = act({ ActivatePaneDirection = "Down" }),
      },
    },
  }
end

-- local tab_keys = {}
-- for i = 1, 8 do
--         table.insert(M.tab_keys, {
--                 -- CTRL+ALT + number to activate that tab
--                 key = tostring(i),
--                 mods = "CTRL|ALT",
--                 action = act.ActivateTab(i - 1),
--         })
-- end

return M
