return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-neotest/neotest-python",
  },
  config = function()
    local python_adapter = require("neotest-python")({
      python = function()
        local whichpy_python = require("whichpy.envs").current_selected()
        if whichpy_python then
          return whichpy_python
        end
        return require("neotest-python.base").get_python_command
      end,
    })
    require("neotest").setup({
      adapters = { python_adapter },
    })
  end,
}
