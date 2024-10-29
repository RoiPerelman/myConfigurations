local M = {}

M.query = {}
M.items = {}
M.indices = {} -- filtered and sorted indices of items
M.matches = {} -- matches
M.caret = 1
M.cursor_line = 1
M.guicursor = ''
M.stop = false
M.visible_range = { from = -1, to = -1 }

M.restore_defaults = function()
  M.query = {}
  M.items = {}
  M.indices = {}
  M.caret = 1
  M.cursor_line = 1
  M.guicursor = ''
  M.stop = false
  M.visible_range = { from = -1, to = -1 }
end

return M
