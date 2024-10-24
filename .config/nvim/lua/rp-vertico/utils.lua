local M = {}

M.set_extmark = function(...) pcall(vim.api.nvim_buf_set_extmark, ...) end

M.replace_termcodes = function(actions)
  local replaced_actions = {}
  for key, value in pairs(actions) do
    -- Replace the term codes and store in the new table
    replaced_actions[vim.api.nvim_replace_termcodes(key, true, true, true)] = value
  end
  return replaced_actions
end

-- hack to hide gui cursor while search is active (taken from mini pick that took it from folke noice)
M.guicursor = nil
M.hide_cursor = function()
  M.guicursor = vim.o.guicursor
  vim.api.nvim_set_hl(0, 'RPVerticoNoGuiCursor', { blend = 100, nocombine = true, default = true })
  vim.o.guicursor = 'a:RPVerticoNoGuiCursor'
end
M.unhide_cursor = function()
  -- bring back cursor
  if M.guicursor == '' then vim.cmd('set guicursor=a: | redraw') end
  pcall(function()
    vim.o.guicursor = M.guicursor
    M.guicursor = nil
  end)
end

return M
