local Rule = require('nvim-autopairs.rule')
local npairs = require('nvim-autopairs')


npairs.setup({
        disable_filetype = { "TelescopePrompt" },
        check_ts = true,
        ts_config = {
                lua = { "string", "source" },
                rust = { "string", "source" }
        }
})


npairs.add_rule(Rule("<", ">", "rust"))
