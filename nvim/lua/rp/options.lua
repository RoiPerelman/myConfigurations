local set = vim.opt

vim.api.nvim_set_keymap("n", "<Space>", "<NOP>", { noremap = true, silent = true })
vim.g.mapleader = " "

-- global options
set.mouse = "a" -- enable mouse in all modes
set.completeopt = "menu,menuone,noselect" -- nvim-compe requirement
set.pumheight = 10 -- popup menu height
set.splitbelow = true -- auto horizontal split to below
set.splitright = true -- auto vertical split to the right
set.tabstop = 2 -- number of spaces that a <tab> in the file counts for
set.shiftwidth = 2 -- Change the number of space characters inserted for indentation
set.expandtab = true -- coverts tabs to spaces
set.smartindent = true -- makes indenting smart
set.showmode = false -- we don't need to see things like -- INSERT -- anymore
set.ignorecase = true -- ignore case in search
set.smartcase = true -- ignore case in search only if no capital letter
set.scrolloff = 8 -- scroll before getting to the last line
set.clipboard = "unnamedplus" -- always use the system clipboard
set.inccommand = "nosplit" -- show effect of command incrementally as you type
set.swapfile = false -- no swap file!
set.listchars = "trail:·,tab:→·,nbsp:•,eol:↲,extends:›,precedes:‹" -- how to show special chars space? →›‹»↲␣·•⟩⟨
set.termguicolors = true -- add term gui colors
set.autowriteall = true -- autowrite so we can move buffers without errors

-- window options
set.list = true -- show listchars
set.number = true -- add number
set.relativenumber = true -- add relative number
set.wrap = false -- do not wrap lines
set.cursorline = true -- highlight cursor line
set.signcolumn = "yes" -- add extra space to the left column for signs
set.colorcolumn = "90" -- color a column at 90

-- TODO fix how to set option from lua vim api and not vim.cmd
vim.cmd("set iskeyword+=-") -- treat dash separated words as a word text object
