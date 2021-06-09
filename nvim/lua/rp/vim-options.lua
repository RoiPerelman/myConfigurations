-- set mapleader before all else
vim.api.nvim_set_keymap('n', '<Space>', '<NOP>', {noremap = true, silent = true})
vim.g.mapleader = ' '

-- global options
vim.o.mouse = "a" -- enable mouse in all modes
vim.o.completeopt = "menuone,noselect" -- nvim-compe requirement
vim.o.pumheight = 10 -- popup menu height
vim.o.splitbelow = true -- auto horizontal split to below
vim.o.splitright = true -- auto vertical split to the right
vim.o.tabstop = 2  -- number of spaces that a <tab> in the file counts for
vim.o.shiftwidth = 2 -- Change the number of space characters inserted for indentation
vim.o.expandtab = true -- coverts tabs to spaces
vim.o.smartindent = true -- makes indenting smart
vim.o.showmode = false -- we don't need to see things like -- INSERT -- anymore
vim.o.ignorecase = true -- ignore case in search
vim.o.smartcase = true -- ignore case in search only if no capital letter
vim.o.scrolloff = 8 -- scroll before getting to the last line
vim.o.clipboard = "unnamedplus" -- always use the system clipboard
vim.o.inccommand = "nosplit" -- show effect of command incrementally as you type
vim.o.swapfile = false -- no swap file!
-- vim.o.listchars = "lead:·,trail:·,space:·,tab:→·,nbsp:•,eol:↲,extends:›,precedes:‹" -- how to show special chars space? →›‹»↲␣·•⟩⟨
vim.o.listchars = "trail:·,tab:→·,nbsp:•,eol:↲,extends:›,precedes:‹" -- how to show special chars space? →›‹»↲␣·•⟩⟨
vim.o.termguicolors = true -- add term gui colors
vim.o.autowriteall = true -- autowrite so we can move buffers without errors
-- vim.o.guifont = "" -- change font

-- window options
vim.wo.list = true -- show listchars
vim.wo.number = true -- add number
vim.wo.relativenumber = true -- add relative number
vim.wo.wrap = false -- do not wrap lines
vim.wo.cursorline = true -- highlight cursor line
vim.wo.signcolumn = "yes" -- add extra space to the left column for signs
vim.wo.colorcolumn = "90" -- color a column at 90

-- TODO fix how to set option from lua vim api and not vim.cmd
vim.cmd('set iskeyword+=-') -- treat dash separated words as a word text object

