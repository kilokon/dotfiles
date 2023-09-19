-- I am helpers.lua and I should live in ~/.config/wezterm/helpers.lua

local wezterm = require("wezterm")

-- This is the module table that we will export
local module = {}

-- This function is private to this module and is not visible
-- outside.
local function private_helper()
	wezterm.log_error("hello!")
end

function module.apply_to_config(config)
	private_helper()

	-- config.color_scheme = 'Batman'
end

-- return our module table
return module
