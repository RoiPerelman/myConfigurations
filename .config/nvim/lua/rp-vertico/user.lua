package.loaded['rp-vertico.utils'] = nil
package.loaded['rp-vertico'] = nil

local rp_vertico = require('rp-vertico')

rp_vertico.setup()

vim.api.nvim_create_user_command("RP", rp_vertico.open, {})
