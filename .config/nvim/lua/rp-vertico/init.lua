local Utils = require('rp-vertico.utils')

local M = {}
local H = {}

function M.setup()
end

M.actions_map = {
  ['<Left>']    = 'caret_left',
  ['<Right>']   = 'caret_right',

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

  ['<C-n>']     = 'move_down',
  ['<C-g>']     = 'move_start',
  ['<C-p>']     = 'move_up',

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

H.actions = {
  stop = function()
  end
}

H.timers = {
  redraw = vim.loop.new_timer(),
}

H.bools = {
  is_waiting_for_getcharstr = nil,
}

H.cache = {
  query = {},
  items = {},
  caret = 1,
  current_ind = nil,

  -- caret = {}
}

function H.replace_termcodes(actions)
  local replaced_actions = {}
  for key, value in pairs(actions) do
    -- Replace the term codes and store in the new table
    replaced_actions[vim.api.nvim_replace_termcodes(key, true, true, true)] = value
  end
  return replaced_actions
end

function H.redraw_scheduled()
  vim.schedule(function()
    buffer = Utils.cache.managed_windows['rp-vertico'].buffer
    vim.print(buffer)
    query = table.concat(H.cache.query, "")

    local current = H.cache.current_ind and tostring(H.cache.current_ind) or '!'
    local total = tostring(#H.cache.items)

    vim.api.nvim_buf_set_lines(buffer, 0, 1, false,
      { current .. '/' .. total .. ' ' .. query })
    vim.notify(vim.inspect(H.cache.items))
    vim.api.nvim_buf_set_lines(buffer, 1, -1, false, H.cache.items)
    vim.cmd('redraw')
  end)
end

function M.open()
  local is_open = Utils.toggle_window_by_name('rp-vertico')
  if is_open then
    H.cache.query = {}
    M.main_loop()
  end
end

function M.main_loop()
  local actions_map = H.replace_termcodes(M.actions_map)

  M.find_files()

  for _ = 1, 1000000 do
    H.timers.redraw:start(0, 1000, H.redraw_scheduled)
    H.cache.is_waiting_for_getcharstr = true
    local ok, char = pcall(vim.fn.getcharstr)
    H.cache.is_waiting_for_getcharstr = nil
    H.timers.redraw:stop()

    local c = vim.api.nvim_replace_termcodes('<C-z>', true, true, true)
    if actions_map[char] then
      vim.notify('special char ' .. char .. ' ' .. actions_map[char])
      goto continue
    end

    -- close using C-c
    if not ok then
      Utils.toggle_window_by_name('rp-vertico')
      break
    end

    table.insert(H.cache.query, char)

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
    vim.print('ROIROI updated cache')
    H.cache.items = items
    H.redraw_scheduled()
  end)
end

return M
