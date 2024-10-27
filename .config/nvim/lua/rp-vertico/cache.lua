local M = {}

M.query = {}
M.items = {}
M.filtered_sorted_items = {}
M.caret = 1
M.cursor_line = nil
M.guicursor = ''
M.stop = false

M.restore_defaults = function()
  M.query = {}
  M.items = {}
  M.caret = 1
  M.cursor_line = nil
  M.guicursor = ''
  M.stop = false
end

return M
