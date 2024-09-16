-- for bash we need 3 things,
-- shellcheck
-- bashls (bashls will automatically call shellcheck for linting & code analysis)
-- shfmt for formatting
vim.list_extend(_G.mason_ensure_installed, { "shellcheck", "bash-language-server", "shfmt" })

_G.lsp_config_servers.bashls = {}

_G.conform_formatters_by_ft.sh = { "shfmt" }

