-- TODO: check it out
return {
  "iamcco/markdown-preview.nvim",
  ft = "markdown",
  config = function()
    vim.fn["mkdp#util#install"]()
  end,
}
