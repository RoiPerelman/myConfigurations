package.loaded['rp-vertico.actions'] = nil
package.loaded['rp-vertico.cache'] = nil
package.loaded['rp-vertico.commands'] = nil
package.loaded['rp-vertico.utils'] = nil
package.loaded['rp-vertico.window'] = nil
package.loaded['rp-vertico'] = nil

local rp_vertico = require('rp-vertico')
local commands = require('rp-vertico.commands')

rp_vertico.setup()

-- vim.api.nvim_create_user_command("RP", commands.find_files, {})

commands.search_grep()
-- commands.find_files()
