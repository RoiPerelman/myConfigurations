local keymap = vim.api.nvim_set_keymap

-- noremap means no recursive mappings which means if we map x to y and y to z. x would still be mapped to y and not z

local opts = {noremap = true, silent = true}
-- arrows are for pussies
keymap('n', '<Up>', '<NOP>', opts)
keymap('n', '<Down>', '<NOP>', opts)
keymap('n', '<Left>', '<NOP>', opts)
keymap('n', '<Right>', '<NOP>', opts)

-- i hate escape
keymap('i', 'jj', '<ESC>', opts)
keymap('i', 'kk', '<ESC>', opts)
keymap('i', 'jk', '<ESC>', opts)
keymap('i', 'kj', '<ESC>', opts)

-- alternate way to save
keymap('n', '<C-s>', ':w<CR>', opts)

-- do not loose visual mode when indenting
keymap('v', '<', '<gv', opts)
keymap('v', '>', '>gv', opts)

-- move lines up and down
vim.cmd([[nnoremap <A-j> :m .+1<CR>==]])
vim.cmd([[nnoremap <A-k> :m .-2<CR>==]])
vim.cmd([[inoremap <A-j> <Esc>:m .+1<CR>==gi]])
vim.cmd([[inoremap <A-k> <Esc>:m .-2<CR>==gi]])
vim.cmd([[vnoremap <A-j> :m '>+1<CR>gv=gv]])
vim.cmd([[vnoremap <A-k> :m '<-2<CR>gv=gv]])

-- next prev key bindings
-- buffers
keymap('n', ']b', ':bnext<CR>', opts)
keymap('n', '[b', ':bprevious<CR>', opts)
-- quickfix and locallist
keymap('n', ']q', ':cnext<CR>', opts)
keymap('n', '[q', ':cprev<CR>', opts)
keymap('n', ']l', ':lnext<CR>', opts)
keymap('n', '[l', ':lprev<CR>', opts)

-- split windows with leader key
keymap('n', '<Leader>ws', ':sp<CR>', opts)
keymap('n', '<Leader>wv', ':vsp<CR>', opts)

-- better window navigation
keymap('n', '<C-h>', ':wincmd h<CR>', opts)
keymap('n', '<C-l>', ':wincmd l<CR>', opts)
keymap('n', '<C-k>', ':wincmd k<CR>', opts)
keymap('n', '<C-j>', ':wincmd j<CR>', opts)
keymap('n', '<Leader>h', ':wincmd h<CR>', opts)
keymap('n', '<Leader>l', ':wincmd l<CR>', opts)
keymap('n', '<Leader>k', ':wincmd k<CR>', opts)
keymap('n', '<Leader>j', ':wincmd j<CR>', opts)

-- resize windows
vim.cmd([[nnoremap <C-Down> :resize -2<CR>]])
vim.cmd([[nnoremap <C-Up> :resize +2<CR>]])
vim.cmd([[nnoremap <C-Right> :vertical resize -2<CR>]])
vim.cmd([[nnoremap <C-Left> :vertical resize +2<CR>]])

-- Make Y yank till end of line
keymap('n', 'Y', 'y$', opts)

-- cut does so to black hole register!!!
keymap('n', 'c', '"_c', opts)
keymap('n', 'C', '"_C', opts)
keymap('v', 'c', '"_c', opts)
keymap('v', 'C', '"_C', opts)

-- Telescope
keymap('n', '<Leader>ft', ':Telescope <CR>', opts)
keymap('n', '<Leader>ff', ':Telescope find_files<CR>', opts)
keymap('n', '<Leader>fg', ':Telescope live_grep<CR>', opts)
keymap('n', '<Leader>fr', ':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > ")})<CR>', opts)
keymap('n', '<Leader>fe', ':Telescope file_browser<CR>', opts)
keymap('n', '<Leader>fd', ':Telescope lsp_document_diagnostics<CR>', opts)
keymap('n', '<Leader>fb', ':Telescope buffers<CR>', opts)
keymap('n', '<Leader>fm', ':Telescope marks<CR>', opts)
keymap('n', '<Leader>fj', ':Telescope jumplist<CR>', opts)
keymap('n', '<Leader>fq', ':Telescope quickfix<CR>', opts)
keymap('n', '<Leader>fl', ':Telescope loclist<CR>', opts)
keymap('n', '<Leader>fk', ':Telescope keymaps<CR>', opts)
keymap('n', '<Leader>fh', ':Telescope help_tags<CR>', opts)

keymap('n', '<C-p>', ':Telescope find_files<CR>', opts)

-- nvim-tree
keymap('n', '<Leader>e', ':NvimTreeToggle<CR>', opts)

-- git blame
keymap('n', '<Leader>gb', ':GitBlameToggle<CR>', opts)

-- vim asterisk * should highlight current word. n, N to move
-- vim.cmd([[map *  <Plug>(asterisk-z*)]])
-- vim.cmd([[map #  <Plug>(asterisk-z#)]])
-- vim.cmd([[map g* <Plug>(asterisk-gz*)]])
-- vim.cmd([[map g# <Plug>(asterisk-gz#)]])

-- fugitive
-- let g:which_key_map['g'] = {'name' : '+git'}
-- let g:which_key_map['g']['b']= [':Gblame', 'git blame']
-- let g:which_key_map['g']['s']= [':Gstatus', 'git status']
-- let g:which_key_map['g']['d']= [':Gdiff', 'git diff']
-- let g:which_key_map['g']['l']= [':Git log', 'git log']
-- let g:which_key_map['g']['c']= [':Gcommit', 'git commit']
-- let g:which_key_map['g']['p']= [':Gpush', 'git push']
-- let g:which_key_map['g']['r']= {'name' : '+resolve'}
-- let g:which_key_map['g']['r']['l']= [':diffget //2', 'resolve left side']
-- let g:which_key_map['g']['r']['r']= [':diffget //3', 'resolve right side']

-- " Better nav for omnicomplete
-- "inoremap <expr> <c-j> ("\<C-n>")
-- inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
-- "inoremap <expr> <c-k> ("\<C-p>")
