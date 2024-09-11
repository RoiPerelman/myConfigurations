vim.list_extend(_G.mason_ensure_installed, { "taplo" })

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        taplo = {},
      },
    },
  },
}
