local M = {}

local creating_window = false

function M.get_scratch_buffer(bufname)
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

function M.display_buffer(bufnr)
  local current_win = vim.api.nvim_get_current_win()

  vim.cmd("botright vertical new")
  local new_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(new_win, bufnr)
  -- Make sure it doesn't affect layout by preserving 'winfixwidth'
  vim.api.nvim_win_set_option(new_win, "winfixwidth", true)
  vim.api.nvim_win_set_option(new_win, "wrap", true)
  vim.api.nvim_win_set_width(new_win, 40)

  -- Return focus to the original window without making the new one the alternate window
  vim.api.nvim_set_current_win(current_win)
end

M.open_rp_messages_window = function()
  local bufnr = M.get_scratch_buffer("rp_messages")
  local windows = vim.fn.win_findbuf(bufnr)
  if #windows == 0 then -- Buffer not displayed in any window
    if not creating_window then
      creating_window = true
      vim.schedule(function()
        M.display_buffer(bufnr)
        creating_window = false
      end)
    end
  end
end

function M.print(...)
  local bufnr = M.get_scratch_buffer("rp_messages")
  local args = { ... }
  local messages = {}

  local function split_lines(s)
    local lines = {}
    for line in s:gmatch("([^\n]+)") do
      table.insert(lines, line)
    end
    return lines
  end
  -- Prepare messages for setting them in the buffer
  for _, arg in pairs(args) do
    if type(arg) == "table" then
      local inspected = vim.inspect(arg)
      local lines = split_lines(inspected)
      for _, line in ipairs(lines) do
        table.insert(messages, line)
      end
    else
      local str = tostring(arg)
      local lines = split_lines(str)
      for _, line in ipairs(lines) do
        table.insert(messages, line)
      end
    end
  end
  vim.api.nvim_buf_set_lines(bufnr, -1, -1, false, messages)
  local windows = vim.fn.win_findbuf(bufnr)
  for _, win_id in ipairs(windows) do
    local line_count = vim.api.nvim_buf_line_count(bufnr)
    vim.api.nvim_win_set_cursor(win_id, { line_count, 0 })
  end
end

vim.api.nvim_create_user_command("Messages", M.open_rp_messages_window, {})

return M
