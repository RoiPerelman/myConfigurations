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
  group = vim.api.nvim_create_augroup("rp-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- TODO - update to lua
vim.cmd([[
  augroup last_edit_position
    autocmd!
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  augroup END
]])
