local M = {}

M.restore_defaults = function()
  M.query = {}
  M.items = {}
  M.caret = 1
  M.cursor_line = nil
  M.guicursor = ''
end

M.query = {}
M.items = {}
M.caret = 1
M.cursor_line = nil
M.guicursor = ''

return M
