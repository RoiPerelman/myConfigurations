vim.api.nvim_create_autocmd("BufWritePre", {
  desc = "remove trailing whitespace on save",
  group = vim.api.nvim_create_augroup("rp_trim_whitespace", { clear = true }),
  pattern = "*",
  callback = function()
    -- Check if this buffer has disabled whitespace trimming
    if vim.b.rp_disable_trim_whitespace then return end
    -- Remove trailing whitespace
    vim.cmd([[%s/\s\+$//e]])
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "highlight when yanking text",
  group = vim.api.nvim_create_augroup("rp_highlight_yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})


vim.cmd([[
  augroup last_edit_position
    autocmd!
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  augroup END
]])
-- vim.api.nvim_create_autocmd("BufReadPost", {
--   group = vim.api.nvim_create_augroup("rp_last_edit_position", { clear = true }),
--   callback = function()
--     local mark = vim.api.nvim_buf_get_mark(0, '"')
--     local lcount = vim.api.nvim_buf_line_count(0)
--     if mark[1] > 0 and mark[1] <= lcount then
--       pcall(vim.api.nvim_win_set_cursor, 0, mark)
--     end
--   end,
-- })
