-- WezTerm configuration
---------------------------------------------------------------

-- print("hello")
local wezterm = require("wezterm")
local wezKeys = require("keymap")
-- local helpers = require("helpers")
-- local session_manager = require("wezterm-session-manager")

local function is_found(str, pattern)
  return string.find(str, pattern) ~= nil
end

local function platform()
  return {
    is_win = is_found(wezterm.target_triple, "windows"),
    is_linux = is_found(wezterm.target_triple, "linux"),
    is_mac = is_found(wezterm.target_triple, "apple"),
  }
end

wezterm.on("update-right-status", function(window, pane)
  window:set_right_status(window:active_workspace())
end)


-- local sesh = require("sesh")
--
-- wezterm.on("augment-command-palette", function(_window, _pane)
--     return {
--         -- Create a session and interactively name it
--         sesh.create,
--         -- Use wezterm's InputSelector to attach to available sessions
--         sesh.attach,
--         --
--         -- ... the rest of your augment-command-palette config
--     }
-- end)

local padding = {
  left = "1cell",
  right = "1cell",
  top = "0.5cell",
  bottom = "2cell",
}

local function is_editor(pane)
  local process_name = pane:get_foreground_process_name()
  if process_name then
    return process_name:find("vim")
  end
  return false
end

local function is_terminal_explorer(pane)
  local process_name = pane:get_foreground_process_name()
  if process_name then
    return process_name:find("yazi")
  end
  return false
end

-- wezterm.on("update-status", function(window, pane)
--   local overrides = window:get_config_overrides() or {}
--   if is_editor(pane) then
--     overrides.window_padding = {
--       left = 0,
--       right = 0,
--       top = 0,
--       bottom = 0,
--     }
--   else
--     overrides.window_padding = padding
--   end
--   window:set_config_overrides(overrides)
-- end)

-- wezterm.on("update-status", function(window, pane)
--   local overrides = window:get_config_overrides() or {}
--   if is_editor(pane) then
--     overrides.window_padding = {
--       left = 0,
--       right = 0,
--       top = 0,
--       bottom = 0,
--     }
--   else
--     overrides.window_padding = padding
--   end
--   window:set_config_overrides(overrides)
-- end)




local config = {}
-- local nf = wezterm.nerdfonts
-- local pane_cwd_cache = {}
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- config.default_prog = {"/usr/bin/nu", "-l"}
config.default_prog = { "/usr/bin/zsh", "-l" }
-- config.default_prog = { "/usr/bin/fish", "-l" }
config.default_cursor_style = "SteadyBar"
config.font = wezterm.font_with_fallback({
  -- {
  -- family = "JetBrains Mono",
  -- family = "Fira Code",
  -- harfbuzz_features = { "calt=0", "clig=0", "liga=0" },
  -- },
  {
    family = "JetBrains Mono",
    harfbuzz_features = { "calt=1", "clig=0", "liga=0" },
  },
  {
    family = "Iosevka",
    harfbuzz_features = { "calt=1", "clig=1", "liga=1" },
  },
  -- { family = "JetBrainsMonoNL Nerd Font Mono" },
  -- { family = "Operator Mono SSm Lig", weight = "Bold" },
})
-- config.font = wezterm.font("RobotoMono Nerd Font")
config.font_size = 11
config.color_scheme = "tokyonight-storm"
config.hide_tab_bar_if_only_one_tab = true
config.window_background_opacity = 0.85
config.text_background_opacity = 0.7
config.disable_default_key_bindings = true
-- config.window_padding = {
--   top = 7,
--   bottom = 2,
--   left = 1,
--   right = 1,
-- }
config.enable_wayland = false
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.tab_max_width = 30
config.window_decorations = "RESIZE"
-- config.leader = { key = "VoidSymbol", mods = "", timeout_milliseconds = 1000 }
config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = wezKeys.keys()
config.key_tables = wezKeys.key_tables()
config.window_frame = {
  -- font = wezterm.font({ family = "Iosevka Aile" }),
  font = wezterm.font({ family = "JetBrains Mono", weight = "Bold" }),
  font_size = 11.0,
  border_left_width = "0.0cell",
  border_right_width = "0.0cell",
  border_bottom_height = "0.10cell",
  border_bottom_color = "#1a1b26",
  border_top_height = "0.0cell",
}
config.underline_thickness = 1
config.underline_position = -2.0

config.launch_menu = {

  {
    label = "Felix Dotfiles",
    args = { "zsh", "-c", "fx", "~/dotfiles" },
  },
  {
    label = "Felix Dev",
    args = { "fx", "~/sync/NO_LOGIC_HERE" },
  },
}



-- Check whether the given file exists
function file_exists(name)
  local f = io.open(name, "r")
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

function wezterm_terminfo_installed()
  return file_exists("/usr/share/terminfo/w/wezterm")
end

-- Determine what to set $TERM to
function determine_term_value()
  if wezterm_terminfo_installed() then
    return "wezterm"
  end
  return "xterm-256color"
end

-- helpers.apply_to_config(config)
-- wezterm.log_info("hellos")

return config
