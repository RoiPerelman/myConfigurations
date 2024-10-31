local Utils = require('rp-vertico.utils')

local M = {}

M.actions = {
  stop = function(cache)
    cache.stop = true
  end,
  caret_left = function(cache)
    if cache.caret > 1 then
      cache.caret = cache.caret - 1
    end
  end,
  caret_right = function(cache)
    if cache.caret <= #cache.query then
      cache.caret = cache.caret + 1
    end
  end,
  cursor_down = function(cache)
    if cache.cursor_line < #cache.indices - 1 then
      cache.cursor_line = cache.cursor_line + 1
    elseif cache.cursor_line == #cache.indices - 1 then
      cache.cursor_line = 1
    end
  end,
  cursor_up = function(cache)
    vim.notify('ROIROI 111')
    if cache.cursor_line > 1 then
      vim.notify('ROIROI 222')
      cache.cursor_line = cache.cursor_line - 1
    elseif cache.cursor_line == 1 then
      vim.notify('ROIROI 333')
      cache.cursor_line = #cache.indices - 1
    end
  end,
  delete_char = function(cache)
    if cache.caret > 1 then
      -- Remove the character before the caret
      table.remove(cache.query, cache.caret - 1)
      -- Move the caret one step left
      cache.caret = cache.caret - 1
    end
  end,
  choose = function(cache, window)
    local window_target = window.alternate_window
    local item = Utils.get_active_item(cache)
    if item.path then
      local norm_path = Utils.norm_path(item.path)
      vim.api.nvim_win_call(window_target, function() pcall(vim.cmd, 'edit ' .. norm_path) end)
      cache.stop = true
    end
  end
}

M.actions_map = {
  ['<Left>']    = M.actions.caret_left,
  ['<Right>']   = M.actions.caret_right,

  ['<CR>']      = M.actions.choose,
  ['<C-s>']     = 'choose_in_split',
  ['<C-t>']     = 'choose_in_tabpage',
  ['<C-v>']     = 'choose_in_vsplit',
  ['<M-CR>']    = 'choose_marked',

  ['<BS>']      = M.actions.delete_char,
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

  ['<Esc>']     = M.actions.stop,

  ['<S-Tab>']   = 'toggle_info',
  ['<Tab>']     = 'toggle_preview',
}

return M
