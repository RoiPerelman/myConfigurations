-- general
vim.api.nvim_set_keymap("n", "<Space>", "<NOP>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.have_nerd_font = true

-- invisible chars
vim.opt.list = true
vim.opt.listchars = { trail = "·", tab = "→ ", nbsp = "␣", extends = "›", precedes = "‹", eol = "↲" } -- how to show special chars →›‹»↲¬␣·•⟩⟨

-- tabs
vim.opt.tabstop = 2 -- a tab is shown as x spaces
vim.opt.shiftwidth = 2 -- pressing tab (shifting text) repr as spaces but inserts tabs
vim.opt.expandtab = true -- a new tab will be spaces instead

-- text
vim.opt.wrap = false -- soft wrap
vim.opt.linebreak = true -- don't break mid-word during wrap
vim.opt.breakindent = true -- indents soft wrap according to first line
vim.opt.showbreak = "…" -- add the symbol in soft wrap
vim.opt.textwidth = 90 -- hard wrap (gq,gw + motion)
-- TODO: create a toggle to add auto format during writing
-- vim.opt.formatoptions += 'ta' -- t for auto wrap text, a for auto format paragraphs

-- search
vim.opt.ignorecase = true -- ignore case in search
vim.opt.smartcase = true -- ignore case in search only if no capital letter

-- view
vim.opt.splitbelow = true -- auto horizontal split to below
vim.opt.splitright = true -- auto vertical split to the right
vim.opt.scrolloff = 3 -- scroll before getting to the last line
vim.opt.number = true -- add number
vim.opt.relativenumber = false -- add relative number
vim.opt.cursorline = true -- highlight cursor line
vim.opt.signcolumn = "yes" -- add extra space to the left column for signs

--completions
vim.opt.completeopt = "menu,menuone,noselect"

-- misc
vim.opt.clipboard = "unnamedplus" -- always use the system clipboard
vim.opt.swapfile = false -- no swap file!
vim.opt.spellfile = vim.fn.stdpath('config') .. '/spell/en.utf-8.add'
vim.opt.undofile = true  -- Save undo history
vim.opt.iskeyword:append("-") -- treat dash separated words as a word text object

-- add filetypes for files
vim.filetype.add({
  pattern = {
    ['~/.inspektoconfig'] = 'bash',
  },
})
