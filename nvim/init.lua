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
  vim.cmd('colorscheme nvcode') --  monokai nvcode kosmikoa
  vim.cmd([[source $HOME/.config/nvim/vim/startify.vim]])
end

