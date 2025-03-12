local formatters_by_ft = {
    lua = { "stylua" },
    sh = { "shfmt" },
    plantuml = { "trim_whitespace" },
    toml = { "taplo" },
}

local prettierd_filetypes = {
    "javascript",
    "javascriptreact",
    "typescript",
    "typescriptreact",
    "vue",
    "css",
    "scss",
    "less",
    "html",
    "json",
    "jsonc",
    "yaml",
    "markdown",
    "markdown.mdx",
    "graphql",
    "handlebars",
}

for _, prettierd_filetype in ipairs(prettierd_filetypes) do
    local existing_ft_formatters = formatters_by_ft[prettierd_filetype]

    if existing_ft_formatters ~= nil then
        table.insert(existing_ft_formatters, "prettierd")
    else
        formatters_by_ft[prettierd_filetype] = { "prettierd" }
    end
end

require("conform").setup({
    formatters_by_ft = formatters_by_ft,
    -- formatters_by_ft = {
    --   lua = { "stylua" },
    --   sh = { "shfmt" },
    -- },
    format_on_save = {
        -- These options will be passed to conform.format()
        timeout_ms = 500,
        lsp_format = "fallback",
    },
})
