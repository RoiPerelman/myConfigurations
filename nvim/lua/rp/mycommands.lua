function RP_view_messages_in_scratch()
  -- Create a new buffer and switch to it
  vim.cmd("enew")
  -- Set the buffer-specific options to make it a scratch buffer
  vim.bo.buftype = "nofile"
  vim.bo.bufhidden = "hide"
  vim.bo.swapfile = false

  -- Get the messages and put them into the buffer
  local messages = vim.api.nvim_exec2("messages", { output = true })
  if messages.output == "" then
    return
  end
  local lines = vim.split(messages.output, "\n", { plain = true }) -- Split the messages into lines
  vim.api.nvim_put(lines, "", false, true)
end

-- Add a command to Neovim to call this function easily
vim.api.nvim_create_user_command("Msg", RP_view_messages_in_scratch, {})

-- Add a command to Neovim to print the current working directory
vim.api.nvim_create_user_command("Cwd", function()
  vim.print(vim.fn.getcwd())
end, {})

-- Add a command to Neovim to print the current file path
vim.api.nvim_create_user_command("Cfile", function()
  vim.print(vim.api.nvim_buf_get_name(0))
end, {})

function RP_execute_visual_lua()
  -- Get the start and end position of the visual selection
  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")

  -- Calculate the range
  local start_line, start_col = start_pos[2], start_pos[3]
  local end_line, end_col = end_pos[2], end_pos[3]

  -- Grab the text from the buffer
  local lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false)

  -- Adjust the first and last line to include only the selected portion
  if #lines > 0 then
    lines[1] = string.sub(lines[1], start_col)
    if #lines == 1 then
      lines[#lines] = string.sub(lines[#lines], 1, end_col - start_col)
    else
      lines[#lines] = string.sub(lines[#lines], 1, end_col)
    end
  end

  -- Concatenate the lines and execute as Lua code
  local code = table.concat(lines, "\n")
  local func, err = load(code)
  if func then
    func()
  else
    print("Error loading Lua code: " .. err)
  end
end

local function RP_create_scratch_buffer()
  -- Create an unlisted scratch buffer
  local buf = vim.api.nvim_create_buf(false, true)

  -- Set buffer options to behave like a scratch buffer
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "hide")
  vim.api.nvim_buf_set_option(buf, "swapfile", false)

  return buf
end

-- Function to create and open a scratch buffer
function RP_open_scratch_at_bottom()
  local buf = RP_create_scratch_buffer()
  -- Optionally, set the window's content
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12" })

  local height = 10
  -- opens a new window at the bottom occupying the full width and move to it
  vim.cmd("botright new")
  local win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win, buf)
  vim.api.nvim_win_set_height(win, height)
  vim.api.nvim_win_set_option(win, "number", false)
end

-- RP_open_scratch_at_bottom()

-- local function is_quickfix_window()
--   local buf_type = vim.api.nvim_buf_get_option(vim.api.nvim_get_current_buf(), "buftype")
--   return buf_type == "quickfix"
-- end
--
-- local function preview_quickfix()
--   if not is_quickfix_window() then
--     return
--   end
--
--   local qf_entry = vim.fn.getqflist({ idx = 0, size = 0, items = 0 })
--   if qf_entry.idx == 0 or qf_entry.size == 0 then
--     return
--   end
--
--   local entry = qf_entry.items[qf_entry.idx]
--   if not entry or not entry.bufnr then
--     return
--   end
--
--   -- Preview the file using a floating window or another suitable method
--   local bufnr = vim.fn.bufnr(entry.filename)
--   if vim.api.nvim_buf_is_loaded(bufnr) then
--     vim.api.nvim_win_set_buf(0, bufnr)
--     vim.api.nvim_win_set_cursor(0, { entry.lnum, 0 })
--   end
-- end
--
-- vim.api.nvim_create_augroup("QuickfixPreview", { clear = true })
-- vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
--   group = "QuickfixPreview",
--   pattern = "*",
--   callback = preview_quickfix,
-- })
