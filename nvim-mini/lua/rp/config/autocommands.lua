-- local rp_preview = require("rp_preview")
-- rp_preview.setup_quickfix_preview()

-- TODO go over lazyvim autocommands
-- local function augroup(name)
--   return vim.api.nvim_create_augroup("rp" .. name, { clear = true })
-- end

vim.cmd([[
  augroup last_edit_position
    autocmd!
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  augroup END
]])

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking text",
  group = vim.api.nvim_create_augroup("rp-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

vim.cmd([[
  augroup remove_trailing_whitespace
    autocmd!
    au BufWritePre * %s/\s\+$//e
  augroup END
]])

