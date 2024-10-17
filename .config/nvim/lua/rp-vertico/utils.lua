local U = {}

U.cache = {
  managed_windows = {
    ['rp-vertico'] = {
      buffer = nil,
      window = nil,
      alternate_window = nil,
    },
  },
}

function U.get_scratch_buffer(bufname)
  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    if vim.fn.bufname(bufnr) == bufname then
      return bufnr
    end
  end

  local bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(bufnr, bufname)
  vim.api.nvim_buf_set_option(bufnr, "buftype", "nofile")
  vim.api.nvim_buf_set_option(bufnr, "bufhidden", "hide")
  vim.api.nvim_buf_set_option(bufnr, "swapfile", false)

  return bufnr
end

function U.display_buffer(bufnr)
  vim.cmd("botright new")
  vim.notify('ROIROI in did botright start')
  local new_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(new_win, bufnr)
  -- Make sure it doesn't affect layout by preserving 'winfixwidth'
  vim.api.nvim_win_set_option(new_win, "winfixheight", true)
  vim.api.nvim_win_set_option(new_win, "wrap", true)
  vim.api.nvim_win_set_height(new_win, 11)
  vim.api.nvim_win_set_cursor(new_win, { 1, 0 })
  vim.notify('ROIROI in did botright new end')
  return new_win
end

U.toggle_window_by_name = function(name)
  local buffer = U.get_scratch_buffer(name)
  local windows = vim.fn.win_findbuf(buffer)
  -- Buffer not displayed in any window
  if #windows == 0 then
    vim.notify('ROIROI in toggle open')
    -- vim.schedule(function()
    U.cache.managed_windows[name].alternate_window = vim.api.nvim_get_current_win()
    U.cache.managed_windows[name].window = U.display_buffer(buffer)
    U.cache.managed_windows[name].buffer = buffer
    -- end)
    return true
  else
    vim.notify('ROIROI in toggle close')
    -- Return focus to the original window without making the new one the alternate window
    if U.cache.managed_windows[name].alternate_window then
      vim.api.nvim_set_current_win(U.cache.managed_windows[name].alternate_window)
    end
    -- Buffer is displayed, so close all windows showing it
    for _, window in ipairs(windows) do
      vim.api.nvim_win_close(window, true)
    end
    -- clear cache
    U.cache.managed_windows[name].alternate_window = nil
    U.cache.managed_windows[name].window = nil
    return false
  end
end

return U
