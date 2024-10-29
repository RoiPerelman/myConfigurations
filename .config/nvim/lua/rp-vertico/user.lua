package.loaded['rp-vertico.actions'] = nil
package.loaded['rp-vertico.cache'] = nil
package.loaded['rp-vertico.commands'] = nil
package.loaded['rp-vertico.fzf'] = nil
package.loaded['rp-vertico.init'] = nil
package.loaded['rp-vertico.utils'] = nil
package.loaded['rp-vertico.window'] = nil
package.loaded['rp-vertico'] = nil

local rp_vertico = require('rp-vertico')
local commands = require('rp-vertico.commands')

rp_vertico.setup()

-- vim.api.nvim_create_user_command("RP", commands.find_files, {})

commands.find_files({ dirs = { '~/myConfigurations', '~/Other' } })
-- commands.search_grep()
--
-- local fzf = require "fzf_lib"
-- vim.print(fzf)
--
-- local line = 'hello'
-- local parsed_prompt = fzf.parse_pattern('oe | l')
-- vim.print(tostring(parsed_prompt))
-- vim.print(fzf.get_score(line, parsed_prompt))
-- vim.print(fzf.get_pos(line, parsed_prompt))
