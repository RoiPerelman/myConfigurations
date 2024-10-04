-- personal nvim config
require("rp/config/options")
require("rp/config/keymappings")
require("rp/config/autocommands")
-- Add plugins - download git repos
require("rp/plugin-manager")

-- Configure plugins
local now, later = MiniDeps.now, MiniDeps.later
now(function() require("rp/colorscheme") end)
now(function() require("rp/plugins/tmux") end)
later(function() require("rp/plugins/treesitter") end)
later(function() require("ts-comments").setup() end)
later(function() require("rp/plugins/telescope") end)
later(function() require("rp/plugins/git") end)
-- mini plugins!
later(function() require("rp/plugins/mini/icons") end)
later(function() require("rp/plugins/mini/ai") end)
later(function() require("rp/plugins/mini/surround") end)
later(function() require("rp/plugins/mini/pairs") end)
later(function() require("rp/plugins/mini/jump") end)
-- later(function() require("rp/plugins/mini/notify") end)
later(function() require("rp/plugins/mini/statusline") end)
later(function() require("rp/plugins/mini/pick") end)
later(function() require("rp/plugins/mini/completion") end)
later(function() require("rp/plugins/mini/hipatterns") end)
later(function() require("rp/plugins/mini/diff") end)
later(function() require("rp/plugins/mini/files") end)
-- language support tools
-- initialize global variable for automatic language tools install
_G.mason_ensure_installed = {}
_G.lsp_config_servers = {}
_G.lsp_config_clients_callbacks = {}
_G.conform_formatters = {}
_G.conform_formatters_by_ft = {}
_G.lint_linters_by_ft = {}
now(function() require("rp/plugins/languages/lua") end)
now(function() require("rp/plugins/languages/python") end)
now(function() require("rp/plugins/languages/typescript") end)
now(function() require("rp/plugins/languages/markdown") end)
now(function() require("rp/plugins/languages/bash") end)
now(function() require("rp/plugins/languages/docker") end)
now(function() require("rp/plugins/languages/json") end)
now(function() require("rp/plugins/languages/yaml") end)
now(function() require("rp/plugins/languages/toml") end)
now(function() require("rp/plugins/mason") end)
now(function() require("rp/plugins/lsp") end)
later(function() require("rp/plugins/format") end)
later(function() require("rp/plugins/lint") end)
