local add = MiniDeps.add

add({ source = "tpope/vim-fugitive" })
add({ source = "sindrets/diffview.nvim" })

vim.keymap.set("n", "<leader>gb", ":Git blame<CR>", { desc = "[G]it [B]lame" })
