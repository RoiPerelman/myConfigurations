--  To check the current status of installed tools :Mason
require("mason").setup()

require("mason-tool-installer").setup({
  ensure_installed = _G.mason_ensure_installed
})
