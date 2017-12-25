" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload/plugged')

    " Themes
    " Plug 'joshdick/onedark.vim'
    Plug 'tomasr/molokai'
    " Surround
    " cs"' - change surrounding "" to ''
    " ysiw<q> - you surround inside word with <q> <q/>
    " dst - delete surrounding tag (for these kinds of tags <>)
    " t stands for tag
    " b stands for bracket (
    " B stands for {
    " :help ys cs or ds for more information
    Plug 'tpope/vim-surround'
    " Better Comments
    " gcc - go comment current
    " gcap - go comment a paragraph
    Plug 'tpope/vim-commentary'
    " Add repeat with '.' to other plugins
    Plug 'tpope/vim-repeat'
    " Git
    Plug 'tpope/vim-fugitive'
    " Plug 'mhinz/vim-signify'
    " [c, ]c to move between hunks
    Plug 'airblade/vim-gitgutter'
    " Text navigation - adds highlights on letters for 'f''F' and 't''T'
    Plug 'unblevable/quick-scope'
    " Syntax Support
    Plug 'sheerun/vim-polyglot'
    " File Explorer - lazy loaded
    Plug 'scrooloose/NERDTree', { 'on': 'NERDTreeToggle' }
    Plug 'Xuyuanp/nerdtree-git-plugin'
    " TODO(ROIP) do i want this? 
    " Status line
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    " Auto pairs for '(' '[' '{'.
    Plug 'jiangmiao/auto-pairs'
    " Rainbow parentheses
    Plug 'kien/rainbow_parentheses.vim'
    " Intellisense - Language Server Protocal
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    " FZF
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    " Which key
    Plug 'liuchengxu/vim-which-key'
    " Make git base the root
    " Plug 'airblade/vim-rooter'
    " change copy paste functionality with registers
      " cutlass - c, cc, C, s, S, d, dd, D, x, X will go to black hole register!
      " added m, mm, M functionality for Cut or Move :D 
    Plug 'svermeulen/vim-cutlass'
      " yoink - override p, P to cycle paste yanks with Ctrl up Ctrl down keys
    Plug 'svermeulen/vim-yoink'
      " subversive - s, ss, S for substitute from clipboard
      " siw - substitute in word (with previous yank)
      " sip - substitute inner paragraph (with previous yank)
      " sie - substitute inner entire buffer
      " space s, ss, S subsitute not from clipboard
      " space cs, css, cS confirm subsitution
      " TODO(ROIP) do i want this?
    Plug 'svermeulen/vim-subversive'
    " View and search LSP symbols and tags. TODO(ROIP) learn about this
    Plug 'liuchengxu/vista.vim'
    " Incremental Search imporved (automatically clear highlights)
    " TODO(ROIP) incrememntal search not working and n and N remove highlight
    Plug 'haya14busa/is.vim'
    " Asterisk behavior change 
    Plug 'haya14busa/vim-asterisk'
    " Snippets
    " to create a snippet from code, use Coc action <leader>la or <leader>lA
    Plug 'honza/vim-snippets'

    call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif

