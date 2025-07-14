--  To check the current status of installed tools :Mason
require("mason").setup()

require("mason-tool-installer").setup({
  ensure_installed = {
    -- lsp's
    "lua-language-server",        -- lua_ls.lua
    "pyright",                    -- pyright.lua
    "ruff",                       -- ruff.lua
    "typescript-language-server", -- ts_ls.lua
    "eslint-lsp",                 -- eslint.lua
    "bash-language-server",       -- bashls.lua
    "json-lsp",                   -- jsonls.lua
    "taplo",                      -- toml.lua
    "yaml-language-server",       -- yamlls.lua
    "dockerfile-language-server", -- docker.lua
    -- linters
    "shellcheck",                 -- bash linter (conform)
    -- formatters
    "shfmt",                      -- bash formatter (lint)
    "prettier",                   -- js,jsx,ts,tsx,json,yaml formatter (lint)
  }
})
