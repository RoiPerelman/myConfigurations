---@brief
--- lets u navigate between nvim and tmux panes using the same shortcut
vim.pack.add({ "https://github.com/alexghergh/nvim-tmux-navigation" })

require("nvim-tmux-navigation").setup({})

vim.keymap.set("n", "<C-h>", "<Cmd>NvimTmuxNavigateLeft<CR>", {})
vim.keymap.set("n", "<C-j>", "<Cmd>NvimTmuxNavigateDown<CR>", {})
vim.keymap.set("n", "<C-k>", "<Cmd>NvimTmuxNavigateUp<CR>", {})
vim.keymap.set("n", "<C-l>", "<Cmd>NvimTmuxNavigateRight<CR>", {})

