local add = MiniDeps.add

add({ source = "0xstepit/flow.nvim" })

require("flow").setup {}

vim.cmd("colorscheme flow")
