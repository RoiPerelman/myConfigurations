--  To check the current status of installed tools :Mason
require("mason").setup()

require("mason-tool-installer").setup({
  ensure_installed = {
    "lua-language-server",
    "pyright",
    "ruff",
  }
})
