if exists('g:vscode')
  echo 'loading vscode init.lua conf'
   " vscode extension
else
  echo 'loading init.lua conf'
  :lua require('init')
endif
