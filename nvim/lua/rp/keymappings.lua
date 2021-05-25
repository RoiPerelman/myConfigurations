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

-- keymap('n', '', 'viwU<ESC>', opts) -- easy uppercase
keymap('n', ']b', ':bnext<CR>', opts) -- move to next buffer
keymap('n', '[b', ':bprevious<CR>', opts) -- move to previous buffer

keymap('n', '<C-s>', ':w<CR>', {noremap = true, silent = true }) -- alternate way to save

keymap('v', '<', '<gv', {noremap = true, silent = true }) -- do not loose visual mode when indenting
keymap('v', '>', '>gv', {noremap = true, silent = true }) -- do not loose visual mode when indenting

-- " TODO better keymapss Use alt + hjkl to resize windows
-- keymap('n', '<Leader>j', ':resize -2<CR>', {noremap = true, silent = true })
-- keymap('n', '<Leader>k', ':resize +2<CR>', {noremap = true, silent = true })
-- keymap('n', '<Leader>h', ':vertical resize -2<CR>', {noremap = true, silent = true })
-- keymap('n', '<Leader>l', ':vertical resize +2<CR>', {noremap = true, silent = true })


-- Telescope
keymap('n', '<C-p>', ':Telescope find_files<CR>', opts)
keymap('n', '<C-P>', ':Telescope commands<CR>', opts)
keymap('n', '<Leader>ff', ':Telescope find_files<CR>', opts)
keymap('n', '<Leader>fg', ':Telescope live_grep<CR>', opts)
keymap('n', '<Leader>fr', ':lua require"telescope.builtin".grep_string({ search = vim.fn.input("Grep for > ")})<CR>', opts)
keymap('n', '<Leader>fd', ':Telescope lsp_document_diagnostics<CR>', opts)
keymap('n', '<Leader>fb', ':Telescope buffers<CR>', opts)
keymap('n', '<Leader>fh', ':Telescope help_tags<CR>', opts)
keymap('n', '<Leader>fm', ':Telescope marks<CR>', opts)
keymap('n', '<Leader>fp', ':Telescope man_pages<CR>', opts)
keymap('n', '<Leader>fq', ':Telescope quickfix<CR>', opts)
keymap('n', '<Leader>fl', ':Telescope loclist<CR>', opts)
keymap('n', '<Leader>fc', ':Telescope colorscheme<CR>', opts)
keymap('n', '<Leader>fj', ':Telescope jumplist<CR>', opts)
keymap('n', '<Leader>fk', ':Telescope keymaps<CR>', opts)
keymap('n', '<Leader>fh', ':Telescope help_tags<CR>', opts)

-- nvim-tree
keymap('n', '<Leader>e', ':NvimTreeToggle<CR>', opts)

-- quickfix
keymap('n', ']q', ':cnext<CR>', opts)
keymap('n', '[q', ':cprev<CR>', opts)

-- location list
keymap('n', ']l', ':lnext<CR>', opts)
keymap('n', '[l', ':lprev<CR>', opts)
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
vim.cmd([[nnoremap <A-j> :m .+1<CR>==]])
vim.cmd([[nnoremap <A-k> :m .-2<CR>==]])
vim.cmd([[inoremap <A-j> <Esc>:m .+1<CR>==gi]])
vim.cmd([[inoremap <A-k> <Esc>:m .-2<CR>==gi]])
vim.cmd([[vnoremap <A-j> :m '>+1<CR>gv=gv]])
vim.cmd([[vnoremap <A-k> :m '<-2<CR>gv=gv]])
