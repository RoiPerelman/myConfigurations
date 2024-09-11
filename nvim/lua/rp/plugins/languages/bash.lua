vim.list_extend(_G.mason_ensure_installed, { "shellcheck", "bashls", "shfmt" })

return {
  -- for bash we need 3 things,
  -- shellcheck
  -- bashls (bashls will automatically call shellcheck for linting & code analysis)
  -- shfmt for formatting
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        bashls = {},
      },
    },
  },
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        sh = { "shfmt" },
      },
    },
  },
}
