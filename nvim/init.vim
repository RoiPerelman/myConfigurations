if exists('g:vscode')
  echo 'ROIROI'
   " vscode extension
else
  echo 'ROIROI 2222'
  :lua require('init')
endif
