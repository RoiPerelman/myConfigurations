-- completions
-- hrsh7th/nvim-compe
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
    ultisnips = true;
  };
}

-- telescope
require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    prompt_position = "top", -- make prompt be at the top
    prompt_prefix = "> ",
    selection_caret = "> ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "ascending", -- results widow sorting from top to bottom
    layout_strategy = "horizontal",
    layout_defaults = {
      horizontal = {
        preview_width = 0.55, -- percentage of width preview takes
        -- width_padding = 0.1,
        -- height_padding = 0.05,
      },
    },
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    file_ignore_patterns = {},
    generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    shorten_path = true,
    winblend = 0,
    width = 0.75,
    preview_cutoff = 120,
    results_height = 1,
    results_width = 0.8,
    border = {},
    borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
    color_devicons = true,
    use_less = true,
    set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
    grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    -- Developer configurations: Not meant for general override
    buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker
  }
}

-- startify
-- vim.cmd([[
--   let g:startify_session_dir = '~/.config/nvim/session'

--   " returns all modified files of the current git repo
--   " `2>/dev/null` makes the command fail quietly, so that when we are not
--   " in a git repo, the list will be empty
--   function! s:gitModified()
--     let files = systemlist('git ls-files -m 2>/dev/null')
--     return map(files, "{'line': v:val, 'path': v:val}")
--   endfunction

--   let g:startify_session_autoload = 1
--   let g:startify_session_delete_buffers = 1
--   let g:startify_change_to_vcs_root = 1
--   let g:startify_fortune_use_unicode = 1
--   let g:startify_session_persistence = 1
--   let g:startify_enable_special = 0

--   let g:startify_lists = [
--     \ { 'type': 'bookmarks', 'header': ['   Bookmarks']                    },
--     \ { 'type': 'sessions',  'header': ['   Sessions']                     },
--     \ { 'type': function('s:gitModified'),  'header': ['   git modified']  },
--     \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
--     \ { 'type': 'files',     'header': ['   Files']                        },
--     \ { 'type': 'commands',  'header': ['   Commands']                     },
--     \ ]

--   let g:startify_bookmarks = [
--     \ { 'm': '~/myConfigurations' },
--     \ { 't': '~/tiny_inspektor' },
--     \ { 'c': '~/tiny_inspektor/sw/fixi_client' },
--     \ { 'f': '~/tiny_inspektor/sw/fixi' },
--     \ { 'db': '~/tiny_inspektor/sw/tiny_database' },
--     \ { 'dc': '~/tiny_inspektor/sw/data_coordinator' },
--     \ { 's': '~/tiny_inspektor/sw/tier2/tiny_std' },
--     \ ]
-- ]])

-- vimwiki
-- change vimwiki path
vim.cmd([[
  let g:vimwiki_list = [{'path': '~/.config/nvim/vimwiki/', 'path_html': '~/.config/nvim/vimwiki/html'}]
  nmap <Leader>wa <Plug>VimwikiUISelect
]])

-- treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",
  highlight = {
    enable = true,
  },
}
