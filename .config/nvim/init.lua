-- personal nvim config
require("rp/config/options")
require("rp/config/keymappings")
require("rp/config/autocommands")
-- Add plugins!
require("rp/plugin-manager")
require("rp/colorscheme")
require("rp/plugins/pre")
require("rp/plugins/navigation")
require("rp/plugins/treesitter")
require("rp/plugins/telescope")
require("rp/plugins/git")
-- mini plugins!
require("rp/plugins/mini/hipatterns")
require("rp/plugins/mini/surround")
require("rp/plugins/mini/notify")
require("rp/plugins/mini/diff")
require("rp/plugins/mini/completion")
-- language support tools
-- initialize global variable for automatic language tools install
_G.mason_ensure_installed = {}
_G.lsp_config_servers = {}
_G.lsp_config_clients_callbacks = {}
_G.conform_formatters = {}
_G.conform_formatters_by_ft = {}
_G.conform_formatters_by_ft = {}
_G.lint_linters_by_ft = {}
require("rp/plugins/languages/lua")
require("rp/plugins/languages/python")
require("rp/plugins/languages/typescript")
require("rp/plugins/languages/markdown")
require("rp/plugins/languages/bash")
require("rp/plugins/languages/docker")
require("rp/plugins/languages/json")
require("rp/plugins/languages/yaml")
require("rp/plugins/languages/toml")
require("rp/plugins/mason")
require("rp/plugins/lsp")
require("rp/plugins/format")
require("rp/plugins/lint")
