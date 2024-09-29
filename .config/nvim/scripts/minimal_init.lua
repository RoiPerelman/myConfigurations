-- Add project root as full path to runtime path (in order to be able to
-- `require()`) modules from this module
vim.cmd([[let &rtp.=','.getcwd()]])


-- Set up 'mini.test' only when calling headless Neovim (like with `make test`)
if #vim.api.nvim_list_uis() == 0 then
  -- Add 'mini.nvim' to 'runtimepath' to be able to use all mini
  -- Assumed that 'mini.nvim' is installed using nvim config
  vim.cmd('set rtp+=~/.local/share/nvim/site/pack/deps/start/mini.nvim')

  -- Ensure persistent color scheme (matters after new default in Neovim 0.10)
  vim.o.background = 'dark'
  require('mini.hues').setup({ background = '#11262d', foreground = '#c0c8cc' })

  -- - Make screenshot tests more robust across Neovim versions
  if vim.fn.has('nvim-0.11') == 1 then
    vim.api.nvim_set_hl(0, 'PmenuMatch', { link = 'Pmenu' })
    vim.api.nvim_set_hl(0, 'PmenuMatchSel', { link = 'PmenuSel' })
  end

  -- Set up 'mini.test'
  require('mini.test').setup()
end
