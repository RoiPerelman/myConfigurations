local M = {}

M.buffer = nil
M.window = nil
M.alternate_window = nil

function M.get_scratch_buffer(bufname)
  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    if vim.fn.bufname(bufnr) == bufname then
      return bufnr
    end
  end
  local bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(bufnr, bufname)
  vim.api.nvim_set_option_value("buftype", "nofile", { buf = bufnr })
  vim.api.nvim_set_option_value("bufhidden", "hide", { buf = bufnr })
  vim.api.nvim_set_option_value("swapfile", false, { buf = bufnr })
  return bufnr
end

function M.display_window(bufnr)
  vim.cmd("botright new")
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, bufnr)
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
  vim.api.nvim_win_set_height(window, 11)
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  return window
end

M.open_search_window = function()
  local name = 'rp-vertico'
  local buffer = M.get_scratch_buffer(name)
  local windows = vim.fn.win_findbuf(buffer)
  -- Buffer not displayed in any window
  if #windows == 0 then
    M.alternate_window = vim.api.nvim_get_current_win()
    M.window = M.display_window(buffer)
    M.buffer = buffer
    return true
  else
    M.close_search_window()
    M.open_search_window()
  end
end

M.close_search_window = function()
  local name = 'rp-vertico'
  local buffer = M.get_scratch_buffer(name)
  local windows = vim.fn.win_findbuf(buffer)
  if #windows > 0 or M.window then
    if M.alternate_window and vim.api.nvim_win_is_valid(M.alternate_window) then
      vim.api.nvim_set_current_win(M.alternate_window)
    end

    if vim.api.nvim_win_is_valid(M.window) then
      vim.api.nvim_win_close(M.window, true)
    end

    for _, window in ipairs(windows) do
      if vim.api.nvim_win_is_valid(M.window) then
        vim.api.nvim_win_close(window, true)
      end
    end

    M.alternate_window = nil
    M.window = nil
    M.buffer = nil
  end
end

return M
