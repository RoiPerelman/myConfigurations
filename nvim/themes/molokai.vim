syntax on
colorscheme molokai

" checks if your terminal has 24-bit color support
if (has("termguicolors"))
    set termguicolors
    hi LineNr ctermbg=NONE guibg=NONE
endif

let g:molokai_original = 1
let g:rehash256 = 1
