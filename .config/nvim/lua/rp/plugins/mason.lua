--  To check the current status of installed tools :Mason
require("mason").setup()

require("mason-tool-installer").setup({
  ensure_installed = {
    "lua-language-server",        -- lua_ls.lua
    "pyright",                    -- pyright.lua
    "ruff",                       -- ruff.lua
    "typescript-language-server", -- ts_ls.lua
    "eslint-lsp"                  -- eslint.lua
  }
})
