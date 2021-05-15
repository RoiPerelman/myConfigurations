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

-- window options
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.wrap = false -- do not wrap lines 
vim.wo.cursorline = true

-- TODO fix how to set option from lua vim api and not vim.cmd
vim.cmd('set iskeyword+=-') -- treat dash separated words as a word text object
vim.cmd('set clipboard+=unnamedplus') -- always use the system clipboard

-- " => Vim Essentials - turned on when using vim
-- filetype on
-- filetype plugin on
-- filetype indent on
-- set ignorecase                          " Ignore case in serach
-- set smartcase                           " Ignore case in search only if not capital letter
-- set scrolloff=1                         " scroll before getting to last line
-- set lazyredraw                          " no redraw while executing macros (good preformace config)
-- set magic                               " better regex (keep it)
-- set ffs=unix,dos,mac                    " file format detection
-- 
-- set nobackup                            " This is recommended by coc
-- set nowritebackup                       " This is recommended by coc
-- set shortmess+=c                        " This is recommended by coc
-- set signcolumn=yes                      " This is recommended by coc
-- 
-- " return to last edit position when opening files!
-- au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
-- " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
-- au! BufWritePost $MYVIMRC source %
-- " You can't stop me
-- cmap w!! w !sudo tee %
