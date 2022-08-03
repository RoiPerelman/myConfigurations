local function keymap(m, k, v)
	-- noremap means no recursive mappings which means if we map x to y and y to z.
	-- x would still be mapped to y and not z
	local opts = { noremap = true, silent = true }
	vim.api.nvim_set_keymap(m, k, v, opts)
end

-- arrows are for pussies
keymap("n", "<Up>", "<NOP>")
keymap("n", "<Down>", "<NOP>")
keymap("n", "<Left>", "<NOP>")
keymap("n", "<Right>", "<NOP>")

-- i hate escape
keymap("i", "jj", "<ESC>")
keymap("i", "kk", "<ESC>")
keymap("i", "jk", "<ESC>")
keymap("i", "kj", "<ESC>")

-- alternate way to save
keymap("n", "<C-s>", ":w<CR>")

-- do not loose visual mode when indenting
keymap("v", "<", "<gv")
keymap("v", ">", ">gv")

-- move lines up and down
vim.cmd([[nnoremap <A-j> :m .+1<CR>==]])
vim.cmd([[nnoremap <A-k> :m .-2<CR>==]])
vim.cmd([[inoremap <A-j> <Esc>:m .+1<CR>==gi]])
vim.cmd([[inoremap <A-k> <Esc>:m .-2<CR>==gi]])
vim.cmd([[vnoremap <A-j> :m '>+1<CR>gv=gv]])
vim.cmd([[vnoremap <A-k> :m '<-2<CR>gv=gv]])

-- next prev key bindings
-- buffers
keymap("n", "]b", ":bnext<CR>")
keymap("n", "[b", ":bprevious<CR>")
-- quickfix and locallist
keymap("n", "]q", ":cnext<CR>")
keymap("n", "[q", ":cprev<CR>")
keymap("n", "]l", ":lnext<CR>")
keymap("n", "[l", ":lprev<CR>")

-- split windows with leader key
keymap("n", "<Leader>ws", ":sp<CR>")
keymap("n", "<Leader>wv", ":vsp<CR>")

-- better window navigation
keymap("n", "<C-h>", ":wincmd h<CR>")
keymap("n", "<C-l>", ":wincmd l<CR>")
keymap("n", "<C-k>", ":wincmd k<CR>")
keymap("n", "<C-j>", ":wincmd j<CR>")
keymap("n", "<Leader>h", ":wincmd h<CR>")
keymap("n", "<Leader>l", ":wincmd l<CR>")
keymap("n", "<Leader>k", ":wincmd k<CR>")
keymap("n", "<Leader>j", ":wincmd j<CR>")

-- resize windows
vim.cmd([[nnoremap <C-Down> :resize -2<CR>]])
vim.cmd([[nnoremap <C-Up> :resize +2<CR>]])
vim.cmd([[nnoremap <C-Right> :vertical resize -2<CR>]])
vim.cmd([[nnoremap <C-Left> :vertical resize +2<CR>]])

-- Make Y yank till end of line
keymap("n", "Y", "y$")

-- cut does so to black hole register!!!
keymap("n", "c", '"_c')
keymap("n", "C", '"_C')
keymap("v", "c", '"_c')
keymap("v", "C", '"_C')

-- Telescope
keymap("n", "<Leader>ft", ":Telescope <CR>")
keymap("n", "<Leader>ff", ":Telescope find_files<CR>")
keymap("n", "<Leader>fg", ":Telescope live_grep<CR>")
keymap(
	"n",
	"<Leader>fr",
	':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > ")})<CR>'
)
keymap("n", "<Leader>fe", ":Telescope file_browser<CR>")
keymap("n", "<Leader>fd", ":Telescope lsp_document_diagnostics<CR>")
keymap("n", "<Leader>fb", ":Telescope buffers<CR>")
keymap("n", "<Leader>fm", ":Telescope marks<CR>")
keymap("n", "<Leader>fj", ":Telescope jumplist<CR>")
keymap("n", "<Leader>fq", ":Telescope quickfix<CR>")
keymap("n", "<Leader>fl", ":Telescope loclist<CR>")
keymap("n", "<Leader>fk", ":Telescope keymaps<CR>")
keymap("n", "<Leader>fh", ":Telescope help_tags<CR>")

keymap("n", "<C-p>", ":Telescope find_files<CR>")

-- nvim-tree
keymap("n", "<Leader>e", ":NvimTreeToggle<CR>")
