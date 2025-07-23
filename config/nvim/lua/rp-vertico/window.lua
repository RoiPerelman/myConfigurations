local M = {}

M.buffer = nil
M.window = nil
M.alternate_window = nil

-- parameters for visible lines in window
M.visible_range = { from = -1, to = -1 }

M.open_search_window = function()
  if M.buffer ~= nil or M.window ~= nil then
    vim.notify('rp-vertico open_search_window creation failed')
    return
  end
  local name = 'rp-vertico-buffer'
  M.alternate_window = vim.api.nvim_get_current_win()
  vim.cmd("botright new")
  M.window = vim.api.nvim_get_current_win()
  local window = M.window
  M.buffer = vim.api.nvim_win_get_buf(window)
  local buffer = M.buffer
  vim.api.nvim_win_set_buf(window, buffer)
  --- set buffer options
  vim.api.nvim_buf_set_name(buffer, name)
  vim.api.nvim_set_option_value("buftype", "nofile", { buf = buffer })
  vim.api.nvim_set_option_value("bufhidden", "hide", { buf = buffer })
  vim.api.nvim_set_option_value("swapfile", false, { buf = buffer })
  -- set window options
  vim.api.nvim_set_option_value("winfixheight", true, { win = window })
  vim.api.nvim_set_option_value("foldenable", false, { win = window })
  vim.api.nvim_set_option_value("foldmethod", "manual", { win = window })
  vim.api.nvim_set_option_value("list", true, { win = window })
  vim.api.nvim_set_option_value("listchars", "extends:…", { win = window })
  vim.api.nvim_set_option_value("scrolloff", 0, { win = window })
  vim.api.nvim_set_option_value("wrap", false, { win = window })
  vim.api.nvim_set_option_value("number", false, { win = window })
  vim.api.nvim_set_option_value("relativenumber", false, { win = window })
  vim.api.nvim_set_option_value("signcolumn", "no", { win = window })
  vim.api.nvim_set_option_value("cursorline", false, { win = window })
  vim.api.nvim_set_option_value("colorcolumn", "", { win = window })
  vim.api.nvim_win_set_height(window, 11)
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
end

M.close_search_window = function()
  if M.alternate_window and vim.api.nvim_win_is_valid(M.alternate_window) then
    vim.api.nvim_set_current_win(M.alternate_window)
  end
  if M.window and vim.api.nvim_win_is_valid(M.window) then
    vim.api.nvim_win_close(M.window, true)
  end
  if M.buffer then
    local windows = vim.fn.win_findbuf(M.buffer)
    if #windows > 0 or M.window then
      for _, window in ipairs(windows) do
        if vim.api.nvim_win_is_valid(M.window) then
          vim.api.nvim_win_close(window, true)
        end
      end
    end
    if vim.api.nvim_buf_is_valid(M.buffer) then
      vim.api.nvim_buf_delete(M.buffer, { force = true })
    end
  end
  M.buffer = nil
  M.window = nil
  M.alternate_window = nil
end

return M
