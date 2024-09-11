vim.list_extend(_G.mason_ensure_installed, { "dockerls", "docker_compose_language_service"  })

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        dockerls = {},
        docker_compose_language_service = {},
      },
    },
  },
}
