if vim.g.vscode then
  -- require'rp/vim-options'
  require'rp/vim-options'
  vim.cmd([[
    xmap gc  <Plug>VSCodeCommentary
    nmap gc  <Plug>VSCodeCommentary
    omap gc  <Plug>VSCodeCommentary
    nmap gcc <Plug>VSCodeCommentaryLine
  ]])
else
  require'rp/plugins'
  require'rp/vim-options'
  require'rp/keymappings'
  require'rp/lsp'
  require'rp/plugins-settings'
  require'rp/function-utils'
  require'rp/autocommands'
  vim.cmd('colorscheme dracula') -- nvcode monokai nvcode kosmikoa
  vim.cmd([[source $HOME/.config/nvim/vim/startify.vim]])
end

