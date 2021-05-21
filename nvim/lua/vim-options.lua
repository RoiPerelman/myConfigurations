-- set mapleader before all else
vim.api.nvim_set_keymap('n', '<Space>', '<NOP>', {noremap = true, silent = true})
vim.g.mapleader = ' '

-- global options
vim.o.mouse = "a" -- enable mouse in all modes
vim.o.completeopt = "menuone,noselect" -- nvim-compe requirement
vim.o.pumheight = 10 -- popup menu height
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.tabstop = 2  -- number of spaces that a <tab> in the file counts for
vim.o.shiftwidth = 2 -- Change the number of space characters inserted for indentation
vim.o.expandtab = true -- coverts tabs to spaces
vim.o.smartindent = true -- makes indenting smart
vim.o.showmode = false -- we don't need to see things like -- INSERT -- anymore
vim.o.ignorecase = true -- ignore case in search
vim.o.smartcase = true -- ignore case in search only if no capital letter
vim.o.scrolloff = 1 -- scroll before getting to the last line
vim.o.clipboard = "unnamedplus" -- always use the system clipboard
vim.o.inccommand = "nosplit"
-- vim.o.guifont = "" -- change font

-- window options
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.wrap = false -- do not wrap lines
vim.wo.cursorline = true

-- TODO fix how to set option from lua vim api and not vim.cmd
vim.cmd('set iskeyword+=-') -- treat dash separated words as a word text object
vim.cmd('colorscheme kosmikoa') -- set colorscheme

-- " return to last edit position when opening files!
-- au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
-- " You can't stop me
-- cmap w!! w !sudo tee %
