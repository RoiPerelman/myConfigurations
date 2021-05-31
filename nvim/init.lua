if vim.g.vscode then
  require'rp/vim-options'
else
  require'rp/plugins'
  require'rp/vim-options'
  require'rp/keymappings'
  require'rp/lsp'
  require'rp/plugins-settings'
  require'rp/function-utils'
  require'rp/autocommands'
  vim.cmd('colorscheme kosmikoa') --  monokai nvcode kosmikoa
  -- vim.cmd([[source $HOME/.config/nvim/plug-config/rainbow-parentheses.vim]])
  vim.cmd([[source $HOME/.config/nvim/plug-config/startify.vim]])



end

