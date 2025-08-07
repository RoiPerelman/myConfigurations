--- Navigation

-- make navigation work with wrapped lines
vim.keymap.set({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "$", "v:count == 0 ? 'g$' : '$'", { desc = "Down", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "^", "v:count == 0 ? 'g^' : '^'", { desc = "Up", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "0", "v:count == 0 ? 'g0' : '0'", { desc = "Up", expr = true, silent = true })

-- move lines up and down
vim.keymap.set("n", "<A-j>", "<cmd>m .+1<cr>==", { desc = "Move Down" })
vim.keymap.set("n", "<A-k>", "<cmd>m .-2<cr>==", { desc = "Move Up" })
vim.keymap.set("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
vim.keymap.set("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
vim.keymap.set("v", "<A-j>", ":m '>+1<cr>gv=gv", { desc = "Move Down" })
vim.keymap.set("v", "<A-k>", ":m '<-2<cr>gv=gv", { desc = "Move Up" })

-- indenting (do not lose visual mode)
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

--- Windows

-- better window navigation See `:help wincmd`
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "focus left window" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "focus right window" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "focus lower window" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "focus upper window" })

-- resize windows
vim.keymap.set("n", "<C-Up>", ":resize +2<cr>", { desc = "inc window Height" })
vim.keymap.set("n", "<C-Down>", ":resize -2<cr>", { desc = "dec window Height" })
vim.keymap.set("n", "<C-Left>", ":vertical resize -2<cr>", { desc = "dec window width" })
vim.keymap.set("n", "<C-Right>", ":vertical resize +2<cr>", { desc = "inc window width" })

--- Explor
vim.keymap.set("n", "<leader>e", ":Explor<cr>", { desc = "[E]xplor" })

--- Nice to have
vim.keymap.set("n", "<leader>R", ":update<cr> :source<cr>", { desc = "[R]e source" })
