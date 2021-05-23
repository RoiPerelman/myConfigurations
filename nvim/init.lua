if vim.g.vscode then
  require'vim-options'
else
  require'plugins'
  require'vim-options'
  require'keymappings'
  require'lsp'
  require'plugins-settings'
  require'function-utils'
  require'autocommands'
end

