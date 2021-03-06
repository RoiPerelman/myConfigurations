" set leader key
let g:mapleader = "\<Space>"

" => Vim Essentials - turned on when using vim
set nocompatible                        " not compatible with vi
filetype on
filetype plugin on
filetype indent on
syntax enable                           " Enables syntax highlighing
set autoindent                          " Good auto indent
set autoread                            " auto read when file is changed from outside
set backspace=indent,eol,start          " backspace fix
set noerrorbells
set novisualbell
set t_vb=
set tm=500
set history=500
set hlsearch
set incsearch
set ruler              			            " Show the cursor position all the time
set wildmenu                            " autocomplete commands
set showcmd                             " command bottom right window
set smarttab                            " Makes tabbing smarter will realize you have 2 vs 4
set whichwrap+=<,>,h,l                  " backspace fix
set nowb
set noswapfile

set hidden                              " Required to keep multiple buffers open multiple buffers
set nowrap                              " Display long lines as just one line
set encoding=utf-8                      " The encoding displayed 
set pumheight=10                        " Makes popup menu smaller
set fileencoding=utf-8                  " The encoding written to file
set cmdheight=2                         " More space for displaying messages
set iskeyword+=-                      	" treat dash separated words as a word text object"
set mouse=a                             " Enable your mouse
set splitbelow                          " Horizontal splits will automatically be below
set splitright                          " Vertical splits will automatically be to the right
set t_Co=256                            " Support 256 colors
set conceallevel=0                      " So that I can see `` in markdown files
set tabstop=2                           " Insert 2 spaces for a tab
set shiftwidth=2                        " Change the number of space characters inserted for indentation
set expandtab                           " Converts tabs to spaces
set smartindent                         " Makes indenting smart
set laststatus=0                        " Always display the status line
set number relativenumber               " Line numbers
set cursorline                          " Enable highlighting of the current line
set background=dark                     " tell vim what the background color looks like
set showtabline=2                       " Always show tabs 
set noshowmode                          " We don't need to see things like -- INSERT -- anymore
set updatetime=300                      " Faster completion
set formatoptions-=cro                  " Stop newline continution of comments
set clipboard=unnamedplus               " Copy paste between vim and everything else - unnamed if not linux
set ignorecase                          " Ignore case in serach
set smartcase                           " Ignore case in search only if not capital letter
set scrolloff=1                         " scroll before getting to last line
set lazyredraw                          " no redraw while executing macros (good preformace config)
set magic                               " better regex (keep it)
set ffs=unix,dos,mac                    " file format detection

set nobackup                            " This is recommended by coc
set nowritebackup                       " This is recommended by coc
set shortmess+=c                        " This is recommended by coc
set signcolumn=yes                      " This is recommended by coc

" return to last edit position when opening files!
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" auto source when writing to init.vm alternatively you can run :source $MYVIMRC
au! BufWritePost $MYVIMRC source %
" You can't stop me
cmap w!! w !sudo tee %

