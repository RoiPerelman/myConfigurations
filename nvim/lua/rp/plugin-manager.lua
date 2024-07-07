local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--branch=stable",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  spec = { import = "rp/plugins" },
  -- automatically check for plugin updates
  checker = {
    enabled = true,
    notify = false, -- do not notify when plugin updates are available
  },
  change_detection = {
    notify = false, -- do not notify when updating personal config
  },
})
