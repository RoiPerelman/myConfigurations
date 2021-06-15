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

-- gitsigns
require('gitsigns').setup()

-- telescope
local actions = require('telescope.actions')
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
    buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker,
    mappings = {
      i = {
        ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
      },
      n = {
        ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
      }
    }
  }
}

-- vimwiki
-- change vimwiki path
vim.cmd([[
  let g:vimwiki_list = [{'path': '~/.config/nvim/vimwiki/', 'path_html': '~/.config/nvim/vimwiki/html'}]
  nmap <Leader>wa <Plug>VimwikiUISelect
]])

-- autopairs
require('nvim-autopairs').setup()

-- treesitter aka ts
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",
  -- colorscheme
  highlight = {
    enable = true,
  },
  -- rainbow
  rainbow = {
    enable = true,
    extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
    max_file_lines = 1000, -- Do not enable for files with more than 1000 lines, int
  },
  -- autotag
  autotag = {
    enable = true,
  },
  -- matchup
  matchup = {
    enable = true,
  },
  -- context commentstring
  context_commentstring = {
    enable = true
  }
}

-- indent-blankline
vim.cmd("let g:indent_blankline_char = '│'")
-- vim.cmd("let g:indent_blankline_char_highlight_list = ['rainbowcol1', 'rainbowcol2', 'rainbowcol3', 'rainbowcol4', 'rainbowcol5', 'rainbowcol6', 'rainbowcol7', ]")

-- git blame
vim.cmd([[let g:gitblame_date_format = '%r']])
vim.cmd([[let g:gitblame_enabled = 0]])
