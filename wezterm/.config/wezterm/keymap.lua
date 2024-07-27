local wezterm = require("wezterm")
local act = wezterm.action
-- local session_manager = require("wezterm-session-manager")

wezterm.on("update-right-status", function(window, pane)
	local name = window:active_key_table()
	if name then
		name = "TABLE: " .. name
	end
	window:set_right_status(name or "")
end)

-- wezterm.on("save_session", function(window)
-- 	session_manager.save_state(window)
-- end)
-- wezterm.on("load_session", function(window)
-- 	session_manager.load_state(window)
-- end)
-- wezterm.on("restore_session", function(window)
-- 	session_manager.restore_state(window)
-- end)

-- local function is_vim(pane)
--   -- this is set by the plugin, and unset on ExitPre in Neovim
--   return pane:get_user_vars().IS_NVIM == 'true'
-- end

local function is_vim(pane)
	-- This gsub is equivalent to POSIX basename(3)
	-- Given "/foo/bar" returns "bar"
	-- Given "c:\\foo\\bar" returns "bar"
	local process_name = string.gsub(pane:get_foreground_process_name(), "(.*[/\\])(.*)", "%2")
	return process_name == "nvim" or process_name == "vim"
end

local direction_keys = {
	Left = "h",
	Down = "j",
	Up = "k",
	Right = "l",
	-- reverse lookup
	h = "Left",
	j = "Down",
	k = "Up",
	l = "Right",
}

local function split_nav(resize_or_move, key)
	return {
		key = key,
		mods = resize_or_move == "resize" and "META" or "CTRL",
		action = wezterm.action_callback(function(win, pane)
			if is_vim(pane) then
				-- pass the keys through to vim/nvim
				win:perform_action({
					SendKey = { key = key, mods = resize_or_move == "resize" and "META" or "CTRL" },
				}, pane)
			else
				if resize_or_move == "resize" then
					win:perform_action({ AdjustPaneSize = { direction_keys[key], 3 } }, pane)
				else
					win:perform_action({ ActivatePaneDirection = direction_keys[key] }, pane)
				end
			end
		end),
	}
end

local M = {}
M.keys = function()
	return {
		-- Session Manager
		-- { key = "S", mods = "LEADER", action = wezterm.action({ EmitEvent = "save_session" }) },
		-- { key = "L", mods = "LEADER", action = wezterm.action({ EmitEvent = "load_session" }) },
		-- { key = "R", mods = "LEADER", action = wezterm.action({ EmitEvent = "restore_session" }) },

		-- Launcher Menu
		{ key = "l", mods = "ALT", action = act.ShowLauncher },
		-- move between split panes
		split_nav("move", "h"),
		split_nav("move", "j"),
		split_nav("move", "k"),
		split_nav("move", "l"),
		-- resize panes
		split_nav("resize", "h"),
		split_nav("resize", "j"),
		split_nav("resize", "k"),
		split_nav("resize", "l"),
		{ key = "Tab", mods = "CTRL", action = act({ ActivateTabRelative = 1 }) },
		{ key = "Enter", mods = "ALT", action = "ToggleFullScreen" },
		{ key = "C", mods = "CTRL", action = act.CopyTo("ClipboardAndPrimarySelection") },
		{ key = "V", mods = "CTRL", action = act.PasteFrom("Clipboard") },
		{ key = "V", mods = "CTRL", action = act.PasteFrom("PrimarySelection") },
		{ key = "k", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Up" }) },
		{ key = "j", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Down" }) },
		{ key = "h", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Left" }) },
		{ key = "l", mods = "LEADER|CTRL", action = act({ ActivatePaneDirection = "Right" }) },
		{ key = "l", mods = "LEADER", action = act.ShowLauncher }, -- },
		{ key = "t", mods = "LEADER", action = act.SpawnCommandInNewTab({}) },
		{ key = "L", mods = "CTRL", action = wezterm.action.ShowDebugOverlay },
		{
			key = "\\",
			mods = "LEADER",
			action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
		},
		{
			key = "-",
			mods = "LEADER",
			-- mods = "LEADER",
			action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
		},

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
		{
			key = "P",
			mods = "CTRL|ALT",
			action = act({
				RotatePanes = "Clockwise",
			}),
		},
		{ key = ")", mods = "CTRL", action = "SpawnWindow" },
		{
			key = "w",
			mods = "LEADER",
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
		{
			key = "P",
			mods = "LEADER",
			action = wezterm.action({ SwitchWorkspaceRelative = -1 }),
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
		{ key = "{", mods = "LEADER", action = act.ActivateTabRelative(-1) },
		{ key = "}", mods = "LEADER", action = act.ActivateTabRelative(1) },
		{ key = "1", mods = "CTRL|ALT", action = act.ActivateTab(0) },
		{ key = "2", mods = "CTRL|ALT", action = act.ActivateTab(1) },
		{ key = "3", mods = "CTRL|ALT", action = act.ActivateTab(2) },
		{ key = "4", mods = "CTRL|ALT", action = act.ActivateTab(3) },
		{ key = "5", mods = "CTRL|ALT", action = act.ActivateTab(4) },
		{ key = "6", mods = "CTRL|ALT", action = act.ActivateTab(5) },
		{ key = "7", mods = "CTRL|ALT", action = act.ActivateTab(6) },
		{ key = "8", mods = "CTRL|ALT", action = act.ActivateTab(7) },
		{
			key = "W",
			-- mods = "CTRL|SHIFT",
			mods = "LEADER",
			action = act.PromptInputLine({
				description = wezterm.format({
					{ Attribute = { Intensity = "Bold" } },
					{ Foreground = { AnsiColor = "Fuchsia" } },
					{ Text = "Enter name for new workspace" },
				}),
				action = wezterm.action_callback(function(window, pane, line)
					-- line will be `nil` if they hit escape without entering anything
					-- An empty string if they just hit enter
					-- Or the actual line of text they wrote
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
M.key_tables = function()
	return {
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
			{ key = "k", action = act({ AdjustPaneSize = { "Up", 1 } }) },

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

return M
