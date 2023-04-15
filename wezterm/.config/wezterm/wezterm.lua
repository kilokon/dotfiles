local wezterm = require("wezterm")
local wezKeys = require("keymap")

local set_environment_variables = {}
-- local act = wezterm.action
-- local mux = wezterm.mux

wezterm.on("update-right-status", function(window, pane)
        window:set_right_status(window:active_workspace())
end)

return {
        default_prog = { "/usr/bin/zsh" },
        set_environment_variables = set_environment_variables,
        -- font = wezterm.font("JetBrains Mono"),
        -- font = wezterm.font("Source Code Pro"),
        font = wezterm.font("RobotoMono Nerd Font"),
        -- font = wezterm.font("Comic Neue Angular"),
        font_size = 14,
        color_scheme = "tokyonight-storm",
        hide_tab_bar_if_only_one_tab = true,
        window_background_opacity = 0.85,
        text_background_opacity = 0.7,
        disable_default_key_bindings = true,
        pane_focus_follows_mouse = true,
        -- All Keys are defined in keymap.lua
        leader = { key = "VoidSymbol", mods = "", timeout_milliseconds = 3000 },
        keys = wezKeys.keys(),
        key_tables = wezKeys.key_tables(),
}
