--Aviik's wezterm config file

local wezterm = require("wezterm")

--local default_prog;
local set_environment_variables = {}

--------------------------------------
local left = 3
local right = 3
local top = 3
local bottom = 3
local window_padding = { left, right, top, bottom }
--------------------------------------

-- local launch_menu = {
--         {args = {"top"},},
--         {label = "Bash",
--          args = {"bash", "-l"},},
-- }

-- if wezterm.target_triple == "x86_64-pc-windows-msvc" then
--   -- Use OSC 7 as per the above example
--   set_environment_variables["prompt"] = "$E]7;file://localhost/$P$E\\$E[32m$T$E[0m $E[35m$P$E[36m$_$G$E[0m "
--   -- use a more ls-like output format for dir
--   set_environment_variables["DIRCMD"] = "/d"
--   -- And inject clink into the command prompt
--   default_prog = {"cmd.exe", "/s", "/k", "c:/clink/clink_x64.exe", "inject", "-q"}
-- end

return {
	default_prog = { "/usr/bin/fish" },
	-- default_cwd = "/home/aviik/.dotfiles/",
	-- launch_menu = launch_menu,
	set_environment_variables = set_environment_variables,
	automatically_reload_config = true,
	window_padding = window_padding,
	hide_tab_bar_if_only_one_tab = true,
	selection_word_boundary = " \t\n{}[]()\"'`,;:@│*",
	font = wezterm.font("Fira Code"),
	--font_size = 17.0,
	--        font = wezterm.font("JetBrains Mono", {weight="Bold", italic=true}),
	color_scheme = "Batman",
	-- send_composed_key_when_left_alt_is_pressed=false,
	-- send_composed_key_when_right_alt_is_pressed=true,
	leader = { key = "a", mods = "CTRL", timeout_milliseconds = 3000 },
	-- leader = { key="VoidSymbol", mods="", timeout_milliseconds=1000 },
	keys = {
		{
			key = "r",
			mods = "LEADER",
			action = wezterm.action({
				ActivateKeyTable = {
					name = "resize_pane",
					one_shot = false,
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
				},
			}),
		},
		{
			key = "|",
			mods = "LEADER|SHIFT",
			action = wezterm.action({ SplitHorizontal = { domain = "CurrentPaneDomain" } }),
		},
		{ key = "-", mods = "LEADER", action = wezterm.action({ SplitVertical = { domain = "CurrentPaneDomain" } }) },
	},
	key_tables = {
		-- Defines the keys that are active in our resize-pane mode.
		-- Since we're likely to want to make multiple adjustments,
		-- we made the activation one_shot=false. We therefore need
		-- to define a key assignment for getting out of this mode.
		-- 'resize_pane' here corresponds to the name="resize_pane" in
		-- the key assignments above.
		resize_pane = {
			{ key = "LeftArrow", action = wezterm.action({ AdjustPaneSize = { "Left", 1 } }) },
			{ key = "h", action = wezterm.action({ AdjustPaneSize = { "Left", 1 } }) },

			{ key = "RightArrow", action = wezterm.action({ AdjustPaneSize = { "Right", 1 } }) },
			{ key = "l", action = wezterm.action({ AdjustPaneSize = { "Right", 1 } }) },

			{ key = "UpArrow", action = wezterm.action({ AdjustPaneSize = { "Up", 1 } }) },
			{ key = "k", action = wezterm.action({ AdjustPaneSize = { "Up", 1 } }) },

			{ key = "DownArrow", action = wezterm.action({ AdjustPaneSize = { "Down", 1 } }) },
			{ key = "j", action = wezterm.action({ AdjustPaneSize = { "Down", 1 } }) },

			-- Cancel the mode by pressing escape
			{ key = "Escape", action = "PopKeyTable" },
		},

		-- Defines the keys that are active in our activate-pane mode.
		-- 'activate_pane' here corresponds to the name="activate_pane" in
		-- the key assignments above.
		activate_pane = {
			{ key = "LeftArrow", action = wezterm.action({ ActivatePaneDirection = "Left" }) },
			{ key = "h", action = wezterm.action({ ActivatePaneDirection = "Left" }) },

			{ key = "RightArrow", action = wezterm.action({ ActivatePaneDirection = "Right" }) },
			{ key = "l", action = wezterm.action({ ActivatePaneDirection = "Right" }) },

			{ key = "UpArrow", action = wezterm.action({ ActivatePaneDirection = "Up" }) },
			{ key = "k", action = wezterm.action({ ActivatePaneDirection = "Up" }) },

			{ key = "DownArrow", action = wezterm.action({ ActivatePaneDirection = "Down" }) },
			{ key = "j", action = wezterm.action({ ActivatePaneDirection = "Down" }) },
		},
	},
}
