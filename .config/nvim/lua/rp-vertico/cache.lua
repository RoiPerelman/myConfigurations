local M = {}

M.restore_defaults = function()
  M.query = {}
  M.items = {}
  M.caret = 1
  M.cursor_line = nil
  M.guicursor = ''
  M.stop = false
end

M.query = {}
M.items = {}
M.caret = 1
M.cursor_line = nil
M.guicursor = ''
M.stop = false

return M
