local add = MiniDeps.add

add({ source = "folke/tokyonight.nvim" })

require("tokyonight").setup({
  style = "night", -- moon is also good
})
vim.cmd.colorscheme("tokyonight")
