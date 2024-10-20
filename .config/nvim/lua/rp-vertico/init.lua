local Window = require('rp-vertico.window')
local Utils = require('rp-vertico.utils')

local M = {}
local H = {}

function M.setup()
end

M.actions = {
  stop = function()
  end,
  caret_left = function()
    if H.cache.caret > 1 then
      H.cache.caret = H.cache.caret - 1
    end
  end,
  caret_right = function()
    if H.cache.caret <= #H.cache.query then
      H.cache.caret = H.cache.caret + 1
    end
  end,
  cursor_down = function()
    vim.notify('ROIROI' .. tostring(H.cache.cursor_line))
    vim.notify('ROIROI' .. tostring(#H.cache.items))
    if H.cache.cursor_line < #H.cache.items - 1 then
      H.cache.cursor_line = H.cache.cursor_line + 1
    elseif H.cache.cursor_line == #H.cache.items - 1 then
      H.cache.cursor_line = 1
    end
  end,
  cursor_up = function()
    if H.cache.cursor_line > 1 then
      H.cache.cursor_line = H.cache.cursor_line - 1
    elseif H.cache.cursor_line == 1 then
      H.cache.cursor_line = #H.cache.items - 1
    end
  end,

}

M.actions_map = {
  ['<Left>']    = M.actions.caret_left,
  ['<Right>']   = M.actions.caret_right,

  ['<CR>']      = 'choose',
  ['<C-s>']     = 'choose_in_split',
  ['<C-t>']     = 'choose_in_tabpage',
  ['<C-v>']     = 'choose_in_vsplit',
  ['<M-CR>']    = 'choose_marked',

  ['<BS>']      = 'delete_char',
  ['<Del>']     = 'delete_char_right',
  ['<C-u>']     = 'delete_left',
  ['<C-w>']     = 'delete_word',

  ['<C-x>']     = 'mark',
  ['<C-a>']     = 'mark_all',

  ['<C-n>']     = M.actions.cursor_down,
  ['<C-g>']     = 'move_start',
  ['<C-p>']     = M.actions.cursor_up,

  ['<C-r>']     = 'paste',

  ['<C-Space>'] = 'refine',
  ['<M-Space>'] = 'refine_marked',

  ['<C-f>']     = 'scroll_down',
  ['<C-h>']     = 'scroll_left',
  ['<C-l>']     = 'scroll_right',
  ['<C-b>']     = 'scroll_up',

  ['<Esc>']     = 'stop',
  ['<C-z>']     = 'stop',

  ['<S-Tab>']   = 'toggle_info',
  ['<Tab>']     = 'toggle_preview',
}

H.timers = {
  draw = vim.loop.new_timer(),
}

H.bools = {
  is_waiting_for_getcharstr = nil,
}

H.cache = {
  query = {},
  items = {},
  caret = 1,
  cursor_line = nil,
  guicursor = '',
}

H.ns_id = vim.api.nvim_create_namespace('RPVertico')
H.extmarks = {
  cursor = nil,
  cursor_line = nil,
}

function H.create_hl_groups()
  vim.api.nvim_set_hl(0, 'RPVerticoCursor', { link = 'Cursor' })
  vim.api.nvim_set_hl(0, 'RPVerticoCursorLine', { link = 'CursorLine' })
  vim.api.nvim_set_hl(0, 'RPVerticoMarkedLine', { link = 'Visual' })
end

function H.set_win_options()
  local window = Window.cache.managed_windows['rp-vertico'].window
  vim.wo[window].foldenable = false
  vim.wo[window].foldmethod = 'manual'
  vim.wo[window].list = true
  vim.wo[window].listchars = 'extends:…'
  vim.wo[window].scrolloff = 0
  vim.wo[window].wrap = false
  vim.wo[window].number = false
  vim.wo[window].relativenumber = false
  vim.wo[window].signcolumn = 'no'
  vim.wo[window].cursorline = false
end

function H.replace_termcodes(actions)
  local replaced_actions = {}
  for key, value in pairs(actions) do
    -- Replace the term codes and store in the new table
    replaced_actions[vim.api.nvim_replace_termcodes(key, true, true, true)] = value
  end
  return replaced_actions
end

function H.draw()
  local buffer = Window.cache.managed_windows['rp-vertico'].buffer
  if buffer == nil then
    return
  end

  if not H.cache.cursor_line and #H.cache.items > 0 then
    H.cache.cursor_line = 1
  end

  local query = table.concat(H.cache.query, '')

  local current = H.cache.cursor_line and tostring(H.cache.cursor_line) or '!'
  local total = tostring(#H.cache.items)
  local position = current .. '/' .. total

  local prefix = position .. ' # '

  vim.api.nvim_buf_set_lines(buffer, 0, 1, false,
    { prefix .. query .. ' ' })
  vim.api.nvim_buf_set_lines(buffer, 1, -1, false, H.cache.items)

  local caret_column = #prefix - 1 + H.cache.caret

  H.extmarks.cursor = Utils.set_extmark(buffer, H.ns_id, 0, caret_column, {
    id = H.extmarks.cursor,
    hl_group = 'RPVerticoCursor',
    end_row = 0,
    end_col = caret_column + 1,
    priority = 300,
  })

  if H.cache.cursor_line then
    local cursor_line = H.cache.cursor_line

    H.extmarks.cursor_line = Utils.set_extmark(buffer, H.ns_id, cursor_line, 0, {
      id = H.extmarks.cursor_line,
      hl_group = 'RPVerticoCursorLine',
      end_row = cursor_line + 1,
      end_col = 0,
      hl_eol = true,
      priority = 300,
    })
  end

  vim.cmd('redraw')
end

function H.hide_cursor()
  -- Hide cursor while picker is active (to not be visible in the window)
  -- This mostly follows a hack from 'folke/noice.nvim'
  H.cache.guicursor = vim.o.guicursor
  vim.api.nvim_set_hl(0, 'RPVerticoNoGuiCursor', { blend = 100, nocombine = true, default = true })
  vim.o.guicursor = 'a:RPVerticoNoGuiCursor'
end

function H.unhide_cursor()
  -- bring back cursor
  if H.cache.guicursor == '' then vim.cmd('set guicursor=a: | redraw') end
  pcall(function() vim.o.guicursor = H.cache.guicursor end)
end

function M.open()
  local is_open = Window.toggle_window_by_name('rp-vertico')
  if is_open then
    H.cache.query = {}
    H.create_hl_groups()
    H.set_win_options()
    H.hide_cursor()
    M.main_loop()
  end
end

function M.main_loop()
  local actions_map = H.replace_termcodes(M.actions_map)

  M.find_files()

  for _ = 1, 1000000 do
    H.timers.draw:start(0, 1000, vim.schedule_wrap(H.draw))
    H.cache.is_waiting_for_getcharstr = true
    local ok, char = pcall(vim.fn.getcharstr)
    H.cache.is_waiting_for_getcharstr = nil
    H.timers.draw:stop()

    local c = vim.api.nvim_replace_termcodes('<C-z>', true, true, true)
    if type(actions_map[char]) == "function" then
      actions_map[char]()
      goto continue
    elseif actions_map[char] then
      vim.notify('special char ' .. char .. ' ' .. actions_map[char])
      goto continue
    end

    -- close using C-c
    if not ok then
      Window.toggle_window_by_name('rp-vertico')
      H.unhide_cursor()
      break
    end

    table.insert(H.cache.query, H.cache.caret, char)
    H.cache.caret = H.cache.caret + 1
    vim.print(vim.inspect(H.cache.query))

    ::continue::
  end
end

function M.find_files()
  local shell_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" }
  local executable, args = shell_command[1], vim.list_slice(shell_command, 2, #shell_command)

  local process, pid, stdout = nil, nil, vim.loop.new_pipe()

  local stdout = vim.loop.new_pipe()
  local options = {
    args = args,
    stdio = { nil, stdout, nil }
  }
  process, pid = vim.loop.spawn(executable, options, function()
    if process and process:is_active() then process:close() end
  end)

  local data_feed = {}
  stdout:read_start(function(err, data)
    assert(not err, err)

    -- fill data_feed with data
    if data ~= nil then return table.insert(data_feed, data) end

    -- create items from full data_feed
    local items = vim.split(table.concat(data_feed), '\n')
    data_feed = nil
    -- and close the pipe
    stdout:close()
    H.cache.items = items
    vim.schedule_wrap(H.draw)()
  end)
end

return M
