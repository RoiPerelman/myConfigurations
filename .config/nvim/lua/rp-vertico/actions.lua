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
  caret_start = function(cache)
    cache.caret = 1
  end,
  caret_end = function(cache)
    cache.caret = #cache.query + 1
  end,
  caret_word_forward = function(cache)
    while cache.caret <= #cache.query and cache.query[cache.caret] ~= ' ' do
      cache.caret = cache.caret + 1
    end
    while cache.caret <= #cache.query and cache.query[cache.caret] == ' ' do
      cache.caret = cache.caret + 1
    end
  end,
  caret_word_backword = function(cache)
    while cache.caret > 1 and cache.query[cache.caret - 1] == ' ' do
      cache.caret = cache.caret - 1
    end
    while cache.caret > 1 and cache.query[cache.caret - 1] ~= ' ' do
      cache.caret = cache.caret - 1
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
    if cache.cursor_line > 1 then
      cache.cursor_line = cache.cursor_line - 1
    elseif cache.cursor_line == 1 then
      cache.cursor_line = #cache.indices - 1
    end
  end,
  delete_char = function(cache)
    if cache.caret > 1 then
      -- Remove the character before the caret
      table.remove(cache.query, cache.caret - 1)
      cache.query_update = true
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
  -- emacsy movements
  ['<C-a>']     = M.actions.caret_start,
  ['<C-e>']     = M.actions.caret_end,
  ['<C-b>']     = M.actions.caret_left,
  ['<C-f>']     = M.actions.caret_right,
  ['<M-b>']     = M.actions.caret_word_backword,
  ['<M-f>']     = M.actions.caret_word_forward,

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
  -- ['<C-a>']     = 'mark_all',

  ['<C-n>']     = M.actions.cursor_down,
  ['<C-g>']     = 'move_start',
  ['<C-p>']     = M.actions.cursor_up,

  ['<C-r>']     = 'paste',

  ['<C-Space>'] = 'refine',
  ['<M-Space>'] = 'refine_marked',

  -- ['<C-f>']     = 'scroll_down',
  -- ['<C-h>']     = 'scroll_left',
  -- ['<C-l>']     = 'scroll_right',
  -- ['<C-b>']     = 'scroll_up',

  ['<Esc>']     = M.actions.stop,

  ['<S-Tab>']   = 'toggle_info',
  ['<Tab>']     = 'toggle_preview',
}

return M
