local Utils = require('rp-vertico.utils')

local M = {}
local H = {}

function M.setup()
end

H.timers = {
  getcharstr = vim.loop.new_timer(),
}

H.cache = {
  is_in_getcharstr = nil,
}

local x = 0
local y = 0
function H.redraw_scheduled()
  vim.schedule(function() -- Ensure this runs in the main event loop
    vim.notify('redrawing ' .. tostring(y) .. ' ' .. tostring(x))
    x = x + 1
  end)
end

function M.toggle()
  local is_open = Utils.toggle_window_by_name('rp-vertico')
  if is_open then
    for _ = 1, 1000000 do
      H.timers.getcharstr:start(0, 1000, H.redraw_scheduled)
      H.cache.is_in_getcharstr = true
      local ok, char = pcall(vim.fn.getcharstr)
      H.cache.is_in_getcharstr = nil
      H.timers.getcharstr:stop()
      y = y + 1
      print('ROIROI char', char)
      if not ok or char == 'Q' then
        Utils.toggle_window_by_name('rp-vertico')
        break
      end
    end
  end
end

return M

--
