local keymap = vim.api.nvim_set_keymap

-- noremap means no recursive mappings which means if we map x to y and y to z. x would still be mapped to y and not z

-- arrows are for pussies
keymap('n', '<Up>', '<NOP>', {noremap = true, silent = true})
keymap('n', '<Down>', '<NOP>', {noremap = true, silent = true})
keymap('n', '<Left>', '<NOP>', {noremap = true, silent = true})
keymap('n', '<Right>', '<NOP>', {noremap = true, silent = true})

-- i hate escape
keymap('i', 'jj', '<ESC>', {noremap = true, silent = true})
keymap('i', 'kk', '<ESC>', {noremap = true, silent = true})
keymap('i', 'jk', '<ESC>', {noremap = true, silent = true})
keymap('i', 'kj', '<ESC>', {noremap = true, silent = true})

-- keymap('n', '', 'viwU<ESC>', {noremap = true, silent = true}) -- easy uppercase
-- keymap('n', '', ':bnext', {noremap = true, silent = true}) -- move to next buffer
-- keymap('n', '', ':bprevious', {noremap = true, silent = true}) -- move to previous buffer
-- TODO doesn't work
keymap('n', '<C-s>', ':w<CR>', {noremap = true, silent = true }) -- alternate way to save

keymap('v', '<', '<gv', {noremap = true, silent = true }) -- do not loose visual mode when indenting
keymap('v', '>', '>gv', {noremap = true, silent = true }) -- do not loose visual mode when indenting

-- " TODO better keymapss Use alt + hjkl to resize windows
-- keymap('n', '<Leader>j', ':resize -2<CR>', {noremap = true, silent = true })
-- keymap('n', '<Leader>k', ':resize +2<CR>', {noremap = true, silent = true })
-- keymap('n', '<Leader>h', ':vertical resize -2<CR>', {noremap = true, silent = true })
-- keymap('n', '<Leader>l', ':vertical resize +2<CR>', {noremap = true, silent = true })


-- Telescope
keymap('n', '<Leader>ff', ':Telescope find_files<CR>', {noremap = true, silent = true})
keymap('n', '<Leader>fg', ':Telescope live_grep<CR>', {noremap = true, silent = true})
keymap('n', '<Leader>fd', ':Telescope current_buffer_fuzzy_find<CR>', {noremap = true, silent = true})
keymap('n', '<Leader>fb', ':Telescope buffers<CR>', {noremap = true, silent = true})
keymap('n', '<Leader>fh', ':Telescope help_tags<CR>', {noremap = true, silent = true})
keymap('n', '<Leader>fm', ':Telescope man_pages<CR>', {noremap = true, silent = true})

-- " Better window navigation
-- "nnoremap <C-h> <C-w>h
-- "nnoremap <C-j> <C-w>j
-- "nnoremap <C-k> <C-w>k
-- "nnoremap <C-l> <C-w>l


-- " Better nav for omnicomplete
-- "inoremap <expr> <c-j> ("\<C-n>")
-- inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
-- "inoremap <expr> <c-k> ("\<C-p>")

-- " TAB in general mode will move to text buffer
-- nnoremap <TAB> :bnext<CR>
-- " SHIFT-TAB will go back
--
-- " <TAB>: completion.
-- nnoremap <S-TAB> :bprevious<CR>

-- " Move lines up and down
vim.cmd([[nnoremap <A-Down> :m .+1<CR>==]])
vim.cmd([[nnoremap <A-Up> :m .-2<CR>==]])
vim.cmd([[inoremap <A-Down> <Esc>:m .+1<CR>==gi]])
vim.cmd([[inoremap <A-Up> <Esc>:m .-2<CR>==gi]])
vim.cmd([[vnoremap <A-Down> :m '>+1<CR>gv=gv]])
vim.cmd([[vnoremap <A-Up> :m '<-2<CR>gv=gv]])
