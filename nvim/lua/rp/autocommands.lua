vim.cmd(
  [[
  augroup last_edit_position
    autocmd!
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  augroup END
]]
)

vim.cmd(
  [[
  augroup highlight_yank
      autocmd!
      au TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=700}
  augroup END
]]
)

vim.cmd([[
  augroup remove_trailing_whitespace
    autocmd!
    au BufWritePre * %s/\s\+$//e
  augroup END
]])

-- vim.create_autocmd('TextYankPost', {
--   group = num_au
--   callback = function()
--     vim.highlight.on_yank({ higroup = 'Visual', timeout = 120 })
--   end,
-- })
