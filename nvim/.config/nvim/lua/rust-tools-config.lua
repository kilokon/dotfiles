-- Update this path
-- local extension_path = HOME .. '/.vscode-oss/extensions/vadimcn.vscode-lldb-1.7.0/'
local extension_path = vim.env.HOME .. '/.vscode-oss/extensions/vadimcn.vscode-lldb-1.7.0/'
local codelldb_path = extension_path .. 'adapter/codelldb'
local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'

-- local opts = {
--     -- ... other configs
--     dap = {
--         adapter = require('rust-tools.dap').get_codelldb_adapter(
--             codelldb_path, liblldb_path)
--     }
-- }
--


-- local M = {}

-- Needed for autocompletion to work
-- _G.rust_tools_get_graphviz_backends = function()
--   return M.options.tools.crate_graph.enabled_graphviz_backends
-- end

local defaults = {
        tools = {
                autoSetHints = true,
                hover_with_actions = true,
                executor = require("rust-tools/executors").termopen,
                on_initialized = nil,
                inlay_hints = {
                        only_current_line = false,
                        only_current_line_autocmd = "CursorHold",
                        show_parameter_hints = true,
                        show_variable_name = false,
                        parameter_hints_prefix = "<- ",
                        other_hints_prefix = "=> ",
                        max_len_align = false,
                        max_len_align_padding = 1,
                        right_align = false,
                        right_align_padding = 7,
                        highlight = "Comment",
                },
        },
        hover_actions = {
                border = {
                        { "╭", "FloatBorder" },
		        { "─", "FloatBorder" },
		        { "╮", "FloatBorder" },
		        { "│", "FloatBorder" },
		        { "╯", "FloatBorder" },
		        { "─", "FloatBorder" },
		        { "╰", "FloatBorder" },
		        { "│", "FloatBorder" },
                },
                auto_focus = false,
        },
        crate_graph = {
                backend = "x11",
                output = nil,
                full = true,
                enabled_graphviz_backends = {
                        "bmp",
                        "cgimage",
			"canon",
			"dot",
			"gv",
			"xdot",
			"xdot1.2",
			"xdot1.4",
			"eps",
			"exr",
			"fig",
			"gd",
			"gd2",
			"gif",
			"gtk",
			"ico",
			"cmap",
			"ismap",
			"imap",
			"cmapx",
			"imap_np",
			"cmapx_np",
			"jpg",
			"jpeg",
			"jpe",
			"jp2",
			"json",
			"json0",
			"dot_json",
			"xdot_json",
			"pdf",
			"pic",
			"pct",
			"pict",
			"plain",
			"plain-ext",
			"png",
			"pov",
			"ps",
			"ps2",
			"psd",
			"sgi",
			"svg",
			"svgz",
			"tga",
			"tiff",
			"tif",
			"tk",
			"vml",
			"vmlz",
			"wbmp",
			"webp",
			"xlib",
			"x11",
                },
        },
        dap = {
		adapter = {
			type = "executable",
			command = "lldb-vscode",
			name = "rt_lldb",
		},
	},
    -- ... other configs
        -- dap = {
        --         adapter = require('rust-tools.dap').get_codelldb_adapter(
        --         codelldb_path, liblldb_path)
        --
        -- }
}

-- Normal setup
require('rust-tools').setup(defaults)
-- RustRunnables
require('rust-tools.runnables').runnables()
-- Command:
-- RustHoverActions
require'rust-tools.hover_actions'.hover_actions()
