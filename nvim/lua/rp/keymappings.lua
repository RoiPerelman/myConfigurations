vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- arrows are for pussies (can use <NOP>)
vim.keymap.set("n", "<left>", '<cmd>echo "Use h to move!!"<CR>')
vim.keymap.set("n", "<right>", '<cmd>echo "Use l to move!!"<CR>')
vim.keymap.set("n", "<up>", '<cmd>echo "Use k to move!!"<CR>')
vim.keymap.set("n", "<down>", '<cmd>echo "Use j to move!!"<CR>')

-- i hate escape
vim.keymap.set("i", "jj", "<ESC>")
vim.keymap.set("i", "kk", "<ESC>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")

-- alternate way to save
vim.keymap.set("n", "<C-s>", ":w<CR>")

-- do not loose visual mode when indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- move lines up and down
vim.cmd([[nnoremap <A-j> :m .+1<CR>==]])
vim.cmd([[nnoremap <A-k> :m .-2<CR>==]])
vim.cmd([[inoremap <A-j> <Esc>:m .+1<CR>==gi]])
vim.cmd([[inoremap <A-k> <Esc>:m .-2<CR>==gi]])
vim.cmd([[vnoremap <A-j> :m '>+1<CR>gv=gv]])
vim.cmd([[vnoremap <A-k> :m '<-2<CR>gv=gv]])

-- next prev key bindings
-- buffers
vim.keymap.set("n", "]b", ":bnext<CR>")
vim.keymap.set("n", "[b", ":bprevious<CR>")
-- quickfix and locallist
vim.keymap.set("n", "]q", ":cnext<CR>")
vim.keymap.set("n", "[q", ":cprev<CR>")
vim.keymap.set("n", "]l", ":lnext<CR>")
vim.keymap.set("n", "[l", ":lprev<CR>")

-- better window navigation See `:help wincmd`
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" }) -- :wincmd h<CR>
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" }) -- :wincmd l<CR>
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" }) -- :wincmd j<CR>
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" }) -- :wincmd k<CR>

-- resize windows
vim.cmd([[nnoremap <C-Down> :resize -2<CR>]])
vim.cmd([[nnoremap <C-Up> :resize +2<CR>]])
vim.cmd([[nnoremap <C-Right> :vertical resize -2<CR>]])
vim.cmd([[nnoremap <C-Left> :vertical resize +2<CR>]])

-- Make Y yank till end of line (Check if still necessary)
vim.keymap.set("n", "Y", "y$")

-- cut does so to black hole register!!!
vim.keymap.set("n", "c", '"_c')
vim.keymap.set("n", "C", '"_C')
vim.keymap.set("v", "c", '"_c')
vim.keymap.set("v", "C", '"_C')

-- Diagnostic keymaps
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous [D]iagnostic message" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next [D]iagnostic message" })
vim.keymap.set("n", "E", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })
-- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })
