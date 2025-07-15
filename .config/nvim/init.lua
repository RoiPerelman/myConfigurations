-- personal nvim config
require("rp/config/options")
require("rp/config/keymappings")
require("rp/config/autocommands")
-- Add plugins (download git repos)
require("rp/plugin-manager")

-- Configure plugins
local now, later = MiniDeps.now, MiniDeps.later

-- mini plugins!
later(function() require("rp/plugins/mini/icons") end)
later(function() require("rp/plugins/mini/pairs") end)
later(function() require("rp/plugins/mini/statusline") end)
later(function() require("rp/plugins/mini/hipatterns") end)
-- later(function() require("rp/plugins/mini/notify") end)
-- later(function() require("rp/plugins/mini/completion") end)
-- later(function() require("rp/plugins/mini/jump") end)
-- later(function() require("rp/plugins/mini/pick") end)
-- later(function() require("rp/plugins/mini/files") end)

-- Add to neovim basic motion functionality
later(function() require("rp/plugins/mini/ai") end)
later(function() require("rp/plugins/mini/splitjoin") end)
later(function() require("rp/plugins/mini/surround") end)

-- TODO: add mini move (selections up and down)
-- TODO: add mini align (formatexp/formatprg alternative)
-- TODO: check http://vimcasts.org/episodes/formatting-text-with-par/
-- TODO: check http://vimcasts.org/episodes/hard-wrapping-text/

now(function() require("rp/colorscheme") end)
now(function() require("rp/plugins/tmux") end)
now(function() require("rp/plugins/snacks") end)
later(function() require("rp/plugins/treesitter") end)
later(function() require("ts-comments").setup() end)
later(function() require("rp/plugins/git") end)
later(function() require("rp/plugins/oil") end)
later(function() require("rp/plugins/completion") end)

-- language support tools
now(function() require("rp/plugins/mason") end)
now(function() require("rp/plugins/lsp") end)
later(function() require("rp/plugins/format") end)
later(function() require("rp/plugins/lint") end)

-- at the end. uses other plugins to enhance them
later(function() require("rp/plugins/treesitter-repeat") end)
