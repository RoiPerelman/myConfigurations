local M = {}

M.actions = {
  stop = function(cache)
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
    if cache.cursor_line < #cache.items - 1 then
      cache.cursor_line = cache.cursor_line + 1
    elseif cache.cursor_line == #cache.items - 1 then
      cache.cursor_line = 1
    end
  end,
  cursor_up = function(cache)
    if cache.cursor_line > 1 then
      cache.cursor_line = cache.cursor_line - 1
    elseif cache.cursor_line == 1 then
      cache.cursor_line = #cache.items - 1
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

return M
