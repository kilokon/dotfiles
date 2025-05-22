return {
  "CRAG666/code_runner.nvim",
  keys = {
    {
      "<F6>",
      function()
        require("code_runner").run_code()
      end,
      desc = "[e]xcute code",
    },
  },
  opts = {
    mode = "term",
    focus = true,
    startinsert = true,
    -- project_path = vim.fn.expand("~/.config/nvim/project_manager.json"),
    filetype = {
      python = "python3 -u '$dir/$fileName'",
      javascript = "node",
      c = {
        "cd $dir &&",
        "gcc -W -pedantic -g -std=c99 -o /tmp/$fileNameWithoutExt $fileName &&",
        "/tmp/$fileNameWithoutExt",
      },
      ---@diagnostic disable-next-line: unused-vararg
      markdown = function(...)
        local preview_cmd = "zathura"
        local hook = require("code_runner.hooks.preview_pdf")
        require("code_runner.hooks.ui").select({
          Marp = function()
            require("code_runner").run_from_fn("marp --theme-set $MARPT -w -p . &$end")
          end,
          Latex = function()
            hook.run({
              command = "pandoc",
              ---@diagnostic disable-next-line:assign-type-mismatch
              args = { "$fileName", "-o", "$tmpFile" },
              preview_cmd = preview_cmd,
            })
          end,
          Beamer = function()
            hook.run({
              command = "pandoc",
              ---@diagnostic disable-next-line:assign-type-mismatch
              args = { "$fileName", "-o", "$tmpFile", "-t beamer" },
              preview_cmd = preview_cmd,
            })
          end,
        })
      end,
    },
  },
}
