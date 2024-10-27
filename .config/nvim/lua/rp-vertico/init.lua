local Window = require('rp-vertico.window')
local Utils = require('rp-vertico.utils')
local Actions = require('rp-vertico.actions')
local Cache = require('rp-vertico.cache')

local M = {}
local H = {}

M.setup = function()
  H.create_hl_groups()
end

--- Open a new window with optional settings.
-- @param opts table Optional parameters for opening the window.
--   - initial_command function: A function to be called initially.
--   - command function: A function to be called later.
--   - filter function: A function to filter results.
--   - sort function: A function to sort results.
--   - resume boolean: Whether to resume the last session.
function M.open(opts)
  local initial_command = opts.initial_command or function() end
  local command = opts.command or function() end
  local fuzzy_command = opts.fuzzy_command or function() end
  local resume = opts.resume or false
  if not resume then
    Cache.restore_defaults()
  end
  Window.open_search_window()
  Utils.hide_cursor()
  local ok, _ = pcall(M.main_loop, initial_command, command, fuzzy_command)
  if not ok then
    Window.close_search_window()
    Utils.unhide_cursor()
  end
end

function M.main_loop(initial_command, command, fuzzy_command)
  local actions_map = Utils.replace_termcodes(Actions.actions_map)

  initial_command(Cache)

  for _ = 1, 1000000 do
    command(Cache)
    fuzzy_command(Cache)
    H.timers.draw:start(0, 100, vim.schedule_wrap(H.draw))
    -- H.bools.is_waiting_for_getcharstr = true
    local ok, char = pcall(vim.fn.getcharstr) -- C-c returns not ok
    -- H.bools.is_waiting_for_getcharstr = nil
    H.timers.draw:stop()

    if not ok then break end

    if type(actions_map[char]) == "function" then
      vim.notify('special function char ' .. char)
      actions_map[char](Cache)
      if Cache.stop then break end
      goto continue
    elseif actions_map[char] then
      vim.notify('special char ' .. char .. ' ' .. actions_map[char])
      goto continue
    end

    table.insert(Cache.query, Cache.caret, char)
    Cache.caret = Cache.caret + 1
    vim.print(vim.inspect(Cache.query))

    ::continue::
  end

  -- close
  Window.close_search_window()
  Utils.unhide_cursor()
end

-- main loop helpers
H.timers = {
  draw = vim.loop.new_timer(),
}

H.bools = {
  is_waiting_for_getcharstr = nil,
}

H.extmarks = {
  ns_id = nil,
  cursor = nil,
  cursor_line = nil,
}

H.create_hl_groups = function()
  H.extmarks.ns_id = vim.api.nvim_create_namespace('RPVertico')
  vim.api.nvim_set_hl(0, 'RPVerticoCursor', { link = 'Cursor' })
  vim.api.nvim_set_hl(0, 'RPVerticoCursorLine', { link = 'CursorLine' })
  vim.api.nvim_set_hl(0, 'RPVerticoMarkedLine', { link = 'Visual' })
end

H.draw = function()
  local buffer = Window.buffer
  if buffer == nil then
    return
  end

  local items = #Cache.filtered_sorted_items > 0 and Cache.filtered_sorted_items or Cache.items

  if not Cache.cursor_line and #items > 0 then
    Cache.cursor_line = 1
  end

  local query = table.concat(Cache.query, '')

  local current = Cache.cursor_line and tostring(Cache.cursor_line) or '!'
  local total = tostring(#items)
  local position = current .. '/' .. total

  local prefix = position .. ' # '

  -- set search and filtered_sorted_items
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { prefix .. query .. ' ' })
  vim.api.nvim_buf_set_lines(buffer, 1, -1, false, items)

  -- set cursor and cursor line
  local caret_column = #prefix - 1 + Cache.caret
  H.extmarks.cursor = Utils.set_extmark(buffer, H.extmarks.ns_id, 0, caret_column, {
    id = H.extmarks.cursor,
    hl_group = 'RPVerticoCursor',
    end_row = 0,
    end_col = caret_column + 1,
    priority = 300,
  })
  if Cache.cursor_line then
    local cursor_line = Cache.cursor_line
    H.extmarks.cursor_line = Utils.set_extmark(buffer, H.extmarks.ns_id, cursor_line, 0, {
      id = H.extmarks.cursor_line,
      hl_group = 'RPVerticoCursorLine',
      end_row = cursor_line + 1,
      end_col = 0,
      hl_eol = true,
      priority = 300,
    })
  end


  vim.api.nvim__redraw({ flush = true, cursor = true })
  -- vim.api.nvim__redraw({ flush = false, cursor = false })
end

return M
