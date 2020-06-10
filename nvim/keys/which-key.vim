" Leader key
let g:mapleader = "\<Space>"

" Register the description dictionary for the prefix first
call which_key#register('<Space>', "g:which_key_map")

" Map leader to which_key
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
vnoremap <silent> <leader> :silent <c-u> :silent WhichKeyVisual '<Space>'<CR>

" Change the colors if you want
" highlight default link WhichKey          Operator
" highlight default link WhichKeySeperator DiffAdded
" highlight default link WhichKeyGroup     Identifier
" highlight default link WhichKeyDesc      Function

" Hide status line

autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler

" Not a fan of floating windows for this
let g:which_key_use_floating_win = 0

" Define prefix dictionary
let g:which_key_map = {}

" Windows
let g:which_key_map['w'] = {'name' : '+windows/+wiki'}
let g:which_key_map['w']['s'] = {'name': '+split'}
let g:which_key_map['w']['s']['v'] = ['<C-W>v', 'vertical']
let g:which_key_map['w']['s']['h'] = ['<C-W>s', 'horizontal']
let g:which_key_map['w']['h'] = ['<C-w>h', 'go to left window']
let g:which_key_map['w']['j'] = ['<C-w>j', 'go to down window']
let g:which_key_map['w']['k'] = ['<C-w>k', 'go to up window']
let g:which_key_map['w']['l'] = ['<C-w>l', 'got to right window']
let g:which_key_map['w']['='] = ['<C-W>=', 'balance windows']

let g:which_key_map['.'] = [':e $MYVIMRC', 'open init']

" :Hover function comes from Coc
let g:which_key_map['i'] = [':Hover', 'hover info']

" Subversive search and replace
" let g:which_key_map['s'] = ['<plug>(SubversiveSubstituteRange)', 'replace']
" let g:which_key_map['ss'] = ['<plug>(SubversiveSubstituteWordRange)', 'replace word']
" let g:which_key_map['css'] = ['<plug>(SubversiveSubstituteWordRangeConfirm)', 'replace confirm word']

let g:which_key_map['S'] = [':Startify', 'startify']

let g:which_key_map['t'] = {'name' : '+toggle'}
let g:which_key_map['t']['e'] = [':NERDTreeToggle', 'explorer']
let g:which_key_map['t']['h'] = [':set hlsearch!', 'highlight']
let g:which_key_map['t']['t'] = [':Vista!!', 'tags']
let g:which_key_map['t']['a'] = ['<Plug>(asterisk-*)', 'asterisk']

let g:which_key_map['g'] = {'name' : '+git'}
let g:which_key_map['g']['b']= [':Gblame', 'git blame']
let g:which_key_map['g']['s']= [':Gstatus', 'git status']
let g:which_key_map['g']['d']= [':Gdiff', 'git diff']
let g:which_key_map['g']['l']= [':Git log', 'git log']
let g:which_key_map['g']['c']= [':Gcommit', 'git commit']
let g:which_key_map['g']['p']= [':Gpush', 'git push']
let g:which_key_map['g']['r']= {'name' : '+resolve'}
let g:which_key_map['g']['r']['l']= [':diffget //2', 'resolve left side']
let g:which_key_map['g']['r']['r']= [':diffget //3', 'resolve right side']
let g:which_key_map['g']['h'] = {'name' : '+hunk'}
let g:which_key_map['g']['h']['d'] = ['<plug>(GitGutterPreviewHunk)', 'diff hunk']
let g:which_key_map['g']['h']['n'] = [':call GitGutterNextHunkCycle()<CR>', 'next hunk']
let g:which_key_map['g']['h']['p'] = [':call GitGutterPrevHunkCycle()<CR>', 'next hunk']
let g:which_key_map['g']['h']['s'] = ['<plug>(GitGutterStageHunk)', 'stage hunk']
let g:which_key_map['g']['h']['u'] = ['<plug>(GitGutterUndoHunk)', 'undo hunk']
let g:which_key_map['g']['h']['f'] = ['<plug>(GitGutterFoldHunk)', 'fold hunks (zr)']

let g:which_key_map['l'] = {'name' : '+lsp'}
let g:which_key_map['l']['a'] = ['<Plug>(coc-codeaction)', 'line action']
let g:which_key_map['l']['A'] = ['<Plug>(coc-codeaction-selected)', 'selected action']
let g:which_key_map['l']['s'] = [':CocList snippets', 'snippets']
