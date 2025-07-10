-- personal nvim config
require("rp/config/options")
require("rp/config/keymappings")
require("rp/config/autocommands")
-- Add plugins - download git repos
require("rp/plugin-manager")

-- require("rp-vertico/user")

-- Configure plugins
local now, later = MiniDeps.now, MiniDeps.later

now(function() require("rp/colorscheme") end)
now(function() require("rp/plugins/tmux") end)
now(function() require("rp/plugins/snacks") end)
later(function() require("rp/plugins/treesitter") end)
later(function() require("ts-comments").setup() end)
later(function() require("rp/plugins/git") end)
later(function() require("rp/plugins/oil") end)
later(function() require("rp/plugins/completion") end)

-- mini plugins!
later(function() require("rp/plugins/mini/icons") end)
later(function() require("rp/plugins/mini/ai") end)
later(function() require("rp/plugins/mini/splitjoin") end)
later(function() require("rp/plugins/mini/surround") end)
later(function() require("rp/plugins/mini/pairs") end)
later(function() require("rp/plugins/mini/statusline") end)
later(function() require("rp/plugins/mini/hipatterns") end)
-- later(function() require("rp/plugins/mini/notify") end)
-- later(function() require("rp/plugins/mini/completion") end)
-- later(function() require("rp/plugins/mini/jump") end)
-- later(function() require("rp/plugins/mini/pick") end)
-- later(function() require("rp/plugins/mini/files") end)

-- language support tools
-- initialize global variable for automatic language tools install
_G.mason_ensure_installed = {}
_G.lsp_config_servers = {}
_G.lsp_config_clients_callbacks = {}
_G.conform_formatters = {}
_G.conform_formatters_by_ft = {}
_G.lint_linters_by_ft = {}
now(function() require("rp/plugins/languages/markdown") end)
now(function() require("rp/plugins/languages/docker") end)
now(function() require("rp/plugins/languages/yaml") end)
now(function() require("rp/plugins/languages/toml") end)
now(function() require("rp/plugins/mason") end)
now(function() require("rp/plugins/lsp") end)
later(function() require("rp/plugins/format") end)
later(function() require("rp/plugins/lint") end)

-- at the end. uses other plugins to enhance them
later(function() require("rp/plugins/treesitter-repeat") end)
