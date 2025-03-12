local wezterm = require("wezterm")
local act = wezterm.action
--
--
--PLugins
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")

local M = {}
M.keys = function()
  return {

    { key = "C", mods = "CTRL", action = act.CopyTo("ClipboardAndPrimarySelection") },
    { key = "V", mods = "CTRL", action = act.PasteFrom("Clipboard") },
    { key = "V", mods = "CTRL", action = act.PasteFrom("PrimarySelection") },
    { key = "t", mods = "LEADER", action = act.SpawnCommandInNewTab({}) },
    { key = "L", mods = "LEADER", action = wezterm.action.ShowDebugOverlay },
    {
      key = "w",
      mods = "LEADER",
      action = wezterm.action.CloseCurrentPane({ confirm = false }),
    },
    { key = ";", mods = "LEADER", action = act.ShowLauncher }, -- },
    {
      key = "P",
      mods = "CTRL",
      action = act.ShowLauncher,
    },
    {
      key = "p",
      mods = "LEADER",
      action = act.PaneSelect({}),
    },
    {
      key = "\\",
      mods = "LEADER",
      action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
    },
    {
      key = "-",
      mods = "LEADER",
      action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
    },

    {
      key = "r",
      mods = "LEADER",
      action = act({
        ActivateKeyTable = { name = "resize_pane", one_shot = false, replace_current = true },
      }),
    },
    -- Plugins
    {
      key = "s",
      mods = "ALT",
      action = wezterm.action_callback(function(win, pane)
        resurrect.state_manager.save_state(resurrect.workspace_state.get_workspace_state())
        resurrect.window_state.save_window_action()
      end),
    },
    {
      key = "r",
      mods = "ALT",
      action = wezterm.action_callback(function(win, pane)
        resurrect.fuzzy_loader.fuzzy_load(win, pane, function(id, label)
          local type = string.match(id, "^([^/]+)") -- match before '/'
          id = string.match(id, "([^/]+)$") -- match after '/'
          id = string.match(id, "(.+)%..+$") -- remove file extention
          local opts = {
            relative = true,
            restore_text = true,
            on_pane_restore = resurrect.tab_state.default_on_pane_restore,
          }
          if type == "workspace" then
            local state = resurrect.state_manager.load_state(id, "workspace")
            resurrect.workspace_state.restore_workspace(state, opts)
          elseif type == "window" then
            local state = resurrect.state_manager.load_state(id, "window")
            resurrect.window_state.restore_window(pane:window(), state, opts)
          elseif type == "tab" then
            local state = resurrect.state_manager.load_state(id, "tab")
            resurrect.tab_state.restore_tab(pane:tab(), state, opts)
          end
        end)
      end),
    },
    {
      key = "s",
      mods = "LEADER",
      action = workspace_switcher.switch_workspace(),
    },
    {
      key = "S",
      mods = "LEADER",
      action = workspace_switcher.switch_to_prev_workspace(),
    },

    {
      key = "W",
      mods = "LEADER",
      action = act.PromptInputLine({
        description = wezterm.format({
          { Attribute = { Intensity = "Bold" } },
          { Foreground = { AnsiColor = "Fuchsia" } },
          { Text = "Enter name for new workspace" },
        }),
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            window:perform_action(
              act.SwitchToWorkspace({
                name = line,
              }),
              pane
            )
          end
        end),
      }),
    },
  }
end

return M
