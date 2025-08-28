-- general
vim.api.nvim_set_keymap("n", "<Space>", "<NOP>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.have_nerd_font = true

-- invisible chars
vim.opt.list = true
vim.opt.listchars = { trail = "·", tab = "→ ", nbsp = "␣", extends = "›", precedes = "‹" } -- how to show special chars →›‹»↲¬␣·•⟩⟨

-- tabs
vim.opt.tabstop = 2      -- a tab is shown as x spaces
vim.opt.shiftwidth = 2   -- pressing tab (shifting text) repr as spaces but inserts tabs
vim.opt.expandtab = true -- a new tab will be spaces instead

-- text
vim.opt.wrap = false -- soft wrap
vim.opt.linebreak = true -- don't break mid-word during wrap
vim.opt.breakindent = true -- indents soft wrap according to first line
vim.opt.showbreak = "…" -- add the symbol in soft wrap
vim.opt.textwidth = 90 -- hard wrap (gq,gw + motion)

-- search
vim.opt.ignorecase = true -- ignore case in search
vim.opt.smartcase = true  -- ignore case in search only if no capital letter

-- view
vim.opt.splitbelow = true     -- auto horizontal split to below
vim.opt.splitright = true     -- auto vertical split to the right
vim.opt.scrolloff = 3         -- scroll before getting to the last line
vim.opt.sidescrolloff = 10    -- scroll before getting to the last column
vim.opt.number = true         -- add number
vim.opt.relativenumber = true -- add relative number
vim.opt.cursorline = true     -- highlight cursor line
vim.opt.colorcolumn = "100"   -- add vertical line at 80 chars
vim.opt.signcolumn = "yes"    -- add extra space to the left column for signs
vim.opt.winborder = "rounded" -- use rounded border for floating windows
vim.opt.pumheight = 10        -- popup menu height

-- completions
vim.opt.completeopt = "menu,menuone,noselect"

-- diff
vim.opt.diffopt:append("vertical")           -- vertical diff
vim.opt.diffopt:append("algorithm:patience") -- better diff algorithm
vim.opt.diffopt:append("linematch:60")       -- better diff highlighting

-- folding
vim.opt.foldmethod = "expr"                          -- use expression for folding
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- use treesitter for folding
vim.opt.foldlevel = 99                               -- open all folds by default

-- misc
vim.opt.clipboard = "unnamedplus" -- always use the system clipboard
vim.opt.swapfile = false          -- no swap file!
vim.opt.spellfile = vim.fn.stdpath('config') .. '/spell/en.utf-8.add'
vim.opt.undofile = true           -- Save undo history
vim.opt.iskeyword:append("-")     -- treat dash separated words as a word text object
vim.opt.autowriteall = true       -- autowrite so we can move buffers without errors
vim.opt.updatetime = 250          -- Decrease update time (completions + CursorHold/I events)

-- add filetypes for files
vim.filetype.add({
  pattern = {
    ['~/.inspektoconfig'] = 'bash',
  },
})
