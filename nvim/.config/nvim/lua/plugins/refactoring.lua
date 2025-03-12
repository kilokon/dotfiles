return {
  "ThePrimeagen/refactoring.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
  },
  lazy = false,
  config = function()
    require("refactoring").setup({
      print_var_statements = {
        -- add a custom print var statement for cpp
        c = {
          'printf("a custom statement %%s %s", %s)',
        },
        cpp = {
          'std::cout << "%s" << std::endl;',
        },
        python = {
          'print("debug path %s")',
        },
      },
      prompt_func_return_type = {
        cpp = true,
        c = true,
        python = true,
      },
      -- prompt for function parameters
      prompt_func_param_type = {
        cpp = true,
        c = true,
        python = true,
      },
      show_success_message = true,
    })
  end,
}
