if vim.g.vscode then
  require'vim-options'
else
  require'vim-options'
  require'keymappings'
  require'plugins'
  require'lsp'
  require'plugins-settings'
  require'function-utils'
end
