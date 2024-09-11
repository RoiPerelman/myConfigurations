vim.list_extend(_G.mason_ensure_installed, { "jsonls" })

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        jsonls = {},
      },
    },
  },
}
