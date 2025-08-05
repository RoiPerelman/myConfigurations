-- remove trailing whitespace on save
vim.api.nvim_create_augroup("rp_trim_whitespace", { clear = true })
vim.api.nvim_create_autocmd("BufWritePre", {
  group = "rp_trim_whitespace",
  pattern = "*",
  callback = function()
    -- Check if this buffer has disabled whitespace trimming
    if vim.b.rp_disable_trim_whitespace then return end
    -- Remove trailing whitespace
    vim.cmd([[%s/\s\+$//e]])
  end,
})

