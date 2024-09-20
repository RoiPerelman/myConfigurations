vim.list_extend(_G.mason_ensure_installed, { "yamlls" })

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        yamlls = {},
      },
    },
  },
}
