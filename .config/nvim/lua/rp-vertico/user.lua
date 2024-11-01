package.loaded['rp-vertico.actions'] = nil
package.loaded['rp-vertico.cache'] = nil
package.loaded['rp-vertico.commands'] = nil
package.loaded['rp-vertico.fzf'] = nil
package.loaded['rp-vertico.init'] = nil
package.loaded['rp-vertico.utils'] = nil
package.loaded['rp-vertico.window'] = nil
package.loaded['rp-vertico.preview'] = nil
package.loaded['rp-vertico'] = nil

local rp_vertico = require('rp-vertico')
local rp_commands = require('rp-vertico.commands')

rp_vertico.setup()

-- vim.api.nvim_create_user_command("RP", commands.find_files, {})

-- rp_commands.find_files({ dirs = { '~/myConfigurations', '~/Other' } })

local inspekto_filepaths = {
  "~/inspekto/tinybox",
  "~/inspekto/inspekto-os",
  "~/inspekto/meta-inspekto",
  "~/inspekto/fixi",
  "~/inspekto/fixi-client",
  "~/inspekto/tiny-database",
  "~/inspekto/tiny-std",
  "~/inspekto/common-std",
  "~/inspekto/data-coordinator",
  "~/inspekto/connectivity",
  "~/inspekto/inspekto-agent",
  "~/inspekto/integration-managers",
  "~/inspekto/agc",
  "~/inspekto/profile-center",
  "~/inspekto/inspekto-agent",
  "~/inspekto/defect-predictor",
  "~/inspekto/defect-detector",
}

vim.keymap.set("n", "<leader>ff", function()
  rp_commands.find_files()
end, { desc = "[F]ind [F]iles cwd" })


vim.keymap.set("n", "<leader>fF", function()
  local buf_root = require("rp.utils.find_buf_root")()
  rp_commands.find_files({ dirs = { buf_root } })
end, { desc = "[F]ind [F]iles buf root" })

vim.keymap.set("n", "<leader>if", function()
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
