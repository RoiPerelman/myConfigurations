vim.list_extend(_G.mason_ensure_installed, { "dockerfile-language-server", "docker-compose-language-service" })

_G.lsp_config_servers.dockerls = {}
_G.lsp_config_servers.docker_compose_language_service = {}
