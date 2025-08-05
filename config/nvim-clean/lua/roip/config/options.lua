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

vim.opt.swapfile = false -- no swap file!

--completions
vim.opt.completeopt = "menu,menuone,noselect" -- nvim-compe requirement

-- add filetypes for files
vim.filetype.add({
  pattern = {
    ['~/.inspektoconfig'] = 'bash',
  },
})
