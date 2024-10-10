local Utils = require('rp-vertico.utils')

local M = {}

function M.setup()
end

function M.toggle()
  Utils.toggle_window_type_once('rp-vertico')
  for _ = 1, 1000000 do
    local ok, char = pcall(vim.fn.getcharstr)
    print('ROIROI ok', ok)
    print('ROIROI char', char)
    if char == 'Q' then
      break
    end
  end
end

return M
