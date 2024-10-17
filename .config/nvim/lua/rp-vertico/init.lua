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
  getcharstr = vim.loop.new_timer(),
}

H.cache = {
  is_in_getcharstr = nil,
  query = {},
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
  vim.schedule(function() -- Ensure this runs in the main event loop
    buffer = Utils.cache.managed_windows['rp-vertico'].buffer
    query = table.concat(H.cache.query, "")
    vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { '> ' .. query })
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

  for _ = 1, 1000000 do
    H.timers.getcharstr:start(0, 1000, H.redraw_scheduled)
    H.cache.is_in_getcharstr = true
    local ok, char = pcall(vim.fn.getcharstr)
    H.cache.is_in_getcharstr = nil
    H.timers.getcharstr:stop()

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

return M

--
