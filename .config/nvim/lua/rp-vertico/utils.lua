local U = {}

U.cache = {
  creating_window = {},
  alternate_window = nil
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
  local new_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(new_win, bufnr)
  -- Make sure it doesn't affect layout by preserving 'winfixwidth'
  vim.api.nvim_win_set_option(new_win, "winfixheight", true)
  vim.api.nvim_win_set_option(new_win, "wrap", true)
  vim.api.nvim_win_set_height(new_win, 11)
end

U.toggle_window_type_once = function(name)
  local bufnr = U.get_scratch_buffer(name)
  local windows = vim.fn.win_findbuf(bufnr)
  -- Buffer not displayed in any window
  if #windows == 0 then
    if not U.cache.creating_window[name] then
      U.cache.creating_window[name] = true
      local current_win = vim.api.nvim_get_current_win()
      U.cache.alternate_window = current_win
      vim.print('ROIROI 0000', U.cache)
      vim.schedule(function()
        U.display_buffer(bufnr)
        U.cache.creating_window[name] = false
      end)
    end
  else
    vim.print('ROIROI 1111', U.cache)
    -- Return focus to the original window without making the new one the alternate window
    if U.cache.alternate_window then
      vim.api.nvim_set_current_win(U.cache.alternate_window)
    end
    -- Buffer is displayed, so close all windows showing it
    for _, win in ipairs(windows) do
      vim.api.nvim_win_close(win, true)
    end
  end
end

return U
