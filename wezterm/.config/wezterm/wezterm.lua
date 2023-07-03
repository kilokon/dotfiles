local wezterm = require("wezterm")
local wezKeys = require("keymap")

-- local nf = wezterm.nerdfonts
-- local pane_cwd_cache = {}




return {
        default_prog = { "/usr/bin/nu" },
        -- set_environment_variables = set_environment_variables,
        default_cursor_style = "SteadyBar", -- SteadyBlock, BlinkingBlock, SteadyUnderline, BlinkingUnderline, SteadyBar, and BlinkingBar
        -- font = wezterm.font("JetBrains Mono"),
        -- font = wezterm.font("Source Code Pro"),
        font = wezterm.font("RobotoMono Nerd Font"),
        -- font = wezterm.font("Comic Neue Angular"),
        font_size = 12,
        color_scheme = "tokyonight-storm",
        hide_tab_bar_if_only_one_tab = true,
        window_background_opacity = 0.85,
        text_background_opacity = 0.7,
        disable_default_key_bindings = true,
        -- pane_focus_follows_mouse = true,
        window_padding = {
                top = 5,
                bottom = 1,
                left = 10,
                right = 10,
        },
        -- All Keys are defined in keymap.lua
        leader = { key = "VoidSymbol", mods = "", timeout_milliseconds = 1000 },
        keys = wezKeys.keys(),
        key_tables = wezKeys.key_tables(),
}
