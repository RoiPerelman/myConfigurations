local Window = require('rp-vertico.window')
local Utils = require('rp-vertico.utils')
local Actions = require('rp-vertico.actions')
local Cache = require('rp-vertico.cache')
local Preview = require('rp-vertico.preview')

local M = {}
local H = {}

M.setup = function()
  H.create_hl_groups()
end

--- Open a new window with optional settings.
-- @param opts table Optional parameters for opening the window.
--   - command function: A function to be called later.
--   - resume boolean: Whether to resume the last session.
function M.open(opts)
  local command = opts.command or function() end
  local close_cb = opts.close_cb or function() end
  local resume = opts.resume or false

  if not resume then
    Cache.restore_defaults()
  end

  Window.open_search_window()
  Utils.hide_cursor()

  -- main part of the loop
  pcall(M.main_loop, command, close_cb)

  -- when loop ends, we close
  close_cb()
  Window.close_search_window()
  Preview.close_preview()
  Utils.unhide_cursor()
end

function M.main_loop(command)
  local actions_map = Utils.replace_termcodes(Actions.actions_map)

  Cache.visible_range = { from = 1, to = vim.api.nvim_win_get_height(Window.window) - 1 }

  local draw_cb = vim.schedule_wrap(H.draw)

  for i = 1, 1000000 do
    if Cache.query_update == true then
      -- command can only update if query is updated
      -- command update should be async - hence draw_cb should be called
      command(i == 1, Cache, draw_cb)
      Cache.query_update = false
    end
    -- wait for key stroke
    local ok, char = pcall(vim.fn.getcharstr) -- C-c returns not ok
    if not ok then break end

    -- check if keystorke has an action attached. if yes - activate action
    if type(actions_map[char]) == "function" then
      actions_map[char](Cache, Window)
      if Cache.stop then break end
      goto continue
    end

    -- if keystroke doesn't have an action attached and single printable character - add to query
    if #char == 1 and vim.fn.strdisplaywidth(char) == 1 then
      -- query is updated and subsequently caret, cursor_line and visible_range
      table.insert(Cache.query, Cache.caret, char)
      Cache.query_update = true
      Cache.caret = Cache.caret + 1
      Cache.cursor_line = 1
      Cache.visible_range = { from = 1, to = vim.api.nvim_win_get_height(Window.window) - 1 }
    end

    ::continue::
    -- draw after every keystroke
    draw_cb()
  end
end

-- main loop helpers
H.extmarks = {
  ns_id = nil,
  cursor = nil,
  cursor_line = nil,
  matches = nil,
}

H.create_hl_groups = function()
  H.extmarks.ns_id = vim.api.nvim_create_namespace('RPVertico')
  vim.api.nvim_set_hl(0, 'RPVerticoCursor', { link = 'Cursor' })
  vim.api.nvim_set_hl(0, 'RPVerticoCursorLine', { link = 'CursorLine' })
  vim.api.nvim_set_hl(0, 'RPVerticoMarkedLine', { link = 'Visual' })
  vim.api.nvim_set_hl(0, 'RPVerticoMatches', { link = 'Special' })
end

H.draw = function()
  local buffer = Window.buffer
  if buffer == nil then
    return
  end

  local final_items = {}

  local items = Cache.items
  local num_of_final_items = #Cache.indices
  local cursor_line = Cache.cursor_line
  local caret = Cache.caret

  if cursor_line > Cache.visible_range.to then
    Cache.visible_range.from = cursor_line - vim.api.nvim_win_get_height(Window.window) + 2
    Cache.visible_range.to = cursor_line
  end

  if cursor_line < Cache.visible_range.from then
    Cache.visible_range.from = cursor_line
    Cache.visible_range.to = cursor_line + vim.api.nvim_win_get_height(Window.window) - 2
  end

  local range_from = Cache.visible_range.from
  local range_to = Cache.visible_range.to

  for i = range_from, range_to do
    local index = Cache.indices[i]
    table.insert(final_items, items[index])
  end

  local query = table.concat(Cache.query, '')

  local current = num_of_final_items and tostring(cursor_line) or '!'
  local total = tostring(num_of_final_items)
  local position = current .. '/' .. total

  local prefix = position .. ' # '

  local lines = vim.tbl_map(function(item) return item.text end, final_items)
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { prefix .. query .. ' ' })
  vim.api.nvim_buf_set_lines(buffer, 1, -1, false, lines)

  -- set cursor and cursor line
  local caret_column = #prefix - 1 + caret
  H.extmarks.cursor = Utils.set_extmark(buffer, H.extmarks.ns_id, 0, caret_column, {
    id = H.extmarks.cursor,
    hl_group = 'RPVerticoCursor',
    end_row = 0,
    end_col = caret_column + 1,
    priority = 300,
  })
  H.extmarks.cursor_line = Utils.set_extmark(buffer, H.extmarks.ns_id, cursor_line - range_from + 1, 0, {
    id = H.extmarks.cursor_line,
    hl_group = 'RPVerticoCursorLine',
    end_row = cursor_line - range_from + 2,
    end_col = 0,
    hl_eol = true,
    priority = 300,
  })

  -- highlight matches
  for line, match in ipairs(Cache.matches) do
    for _, col in ipairs(match) do
      H.extmarks.matches = Utils.set_extmark(buffer, H.extmarks.ns_id, line, col - 1, {
        id = H.extmarks.matches,
        hl_group = 'RPVerticoMatches',
        end_col = col,
        priority = 300,
      })
    end
  end

  local item = Utils.get_active_item(Cache)
  if item then
    Preview.preview_item(Window.alternate_window, item)
  end

  vim.api.nvim__redraw({ flush = true, cursor = true })
  -- vim.api.nvim__redraw({ flush = false, cursor = false })
end

return M
