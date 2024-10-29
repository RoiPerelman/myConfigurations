package.loaded['rp-vertico.actions'] = nil
package.loaded['rp-vertico.cache'] = nil
package.loaded['rp-vertico.commands'] = nil
package.loaded['rp-vertico.fzf'] = nil
package.loaded['rp-vertico.init'] = nil
package.loaded['rp-vertico.utils'] = nil
package.loaded['rp-vertico.window'] = nil
package.loaded['rp-vertico'] = nil

local rp_vertico = require('rp-vertico')
local rp_commands = require('rp-vertico.commands')

rp_vertico.setup()

-- vim.api.nvim_create_user_command("RP", commands.find_files, {})

-- rp_commands.find_files({ dirs = { '~/myConfigurations', '~/Other' } })

local inspekto_filepaths = {
  "~/sinspekto/tinybox",
  "~/sinspekto/inspekto-os",
  "~/sinspekto/meta-inspekto",
  "~/sinspekto/fixi",
  "~/sinspekto/fixi-client",
  "~/sinspekto/tiny-database",
  "~/sinspekto/tiny-std",
  "~/sinspekto/common-std",
  "~/sinspekto/data-coordinator",
  "~/sinspekto/connectivity",
  "~/sinspekto/inspekto-agent",
  "~/sinspekto/integration-managers",
  "~/sinspekto/agc",
  "~/sinspekto/profile-center",
  "~/sinspekto/inspekto-agent",
  "~/sinspekto/defect-predictor",
  "~/sinspekto/defect-detector",
}

vim.keymap.set("n", "<leader>rf", function()
  rp_commands.find_files()
end, { desc = "RP [F]ind [F]iles cwd" })

vim.keymap.set("n", "<leader>rif", function()
  rp_commands.find_files({ dirs = inspekto_filepaths })
end, { desc = "RP [F]ind [F]iles cwd" })

-- commands.search_grep()
--
-- local fzf = require "fzf_lib"
-- vim.print(fzf)
--
-- local line = 'hello'
-- local parsed_prompt = fzf.parse_pattern('oe | l')
-- vim.print(tostring(parsed_prompt))
-- vim.print(fzf.get_score(line, parsed_prompt))
-- vim.print(fzf.get_pos(line, parsed_prompt
