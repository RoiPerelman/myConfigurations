vim.api.nvim_set_keymap("n", "<Space>", "<NOP>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.have_nerd_font = true

-- global options
vim.opt.mouse = "a" -- enable mouse in all modes
vim.opt.completeopt = "menu,menuone,noselect" -- nvim-compe requirement
vim.opt.pumheight = 10 -- popup menu height
vim.opt.splitbelow = true -- auto horizontal split to below
vim.opt.splitright = true -- auto vertical split to the right
vim.opt.tabstop = 2 -- number of spaces that a <tab> in the file counts for
vim.opt.shiftwidth = 2 -- Change the number of space characters inserted for indentation
vim.opt.expandtab = true -- coverts tabs to spaces
vim.opt.smartindent = true -- makes indenting smart
vim.opt.showmode = false -- we don't need to see things like -- INSERT -- anymore
vim.opt.ignorecase = true -- ignore case in search
vim.opt.smartcase = true -- ignore case in search only if no capital letter
vim.opt.scrolloff = 8 -- scroll before getting to the last line
vim.opt.clipboard = "unnamedplus" -- always use the system clipboard
-- vim.opt.inccommand = "nosplit" -- show effect of command incrementally as you type
vim.opt.inccommand = "split" -- show effect of command incrementally as you type with preview
vim.opt.hlsearch = true
vim.opt.swapfile = false -- no swap file!
vim.opt.termguicolors = true -- add term gui colors
vim.opt.autowriteall = true -- autowrite so we can move buffers without errors
vim.opt.updatetime = 250 -- Decrease update time
vim.opt.timeoutlen = 300 -- Decrease mapped sequence wait time (displays which-key popup sooner)
vim.opt.listchars = { trail = "·", tab = "→ ", nbsp = "␣", extends = "›", precedes = "‹" } -- how to show special chars space? →›‹»↲␣·•⟩⟨

-- window options
vim.opt.list = true -- show listchars
vim.opt.number = true -- add number
vim.opt.relativenumber = false -- add relative number
vim.opt.wrap = false -- do not wrap lines
vim.opt.cursorline = true -- highlight cursor line
vim.opt.signcolumn = "yes" -- add extra space to the left column for signs
vim.opt.colorcolumn = "120" -- color a column at 90
vim.opt.breakindent = true -- Enable break indent

-- buffer options
vim.opt.undofile = true -- Save undo history

-- TODO: fix how to set option from lua vim api and not vim.cmd
vim.cmd("set iskeyword+=-") -- treat dash separated words as a word text object
