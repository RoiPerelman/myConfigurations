" toggle between past copies 
nmap <c-down> <plug>(YoinkPostPasteSwapBack)
nmap <c-up> <plug>(YoinkPostPasteSwapForward)

" change p to use yoink paste
nmap p <plug>(YoinkPaste_p)
nmap P <plug>(YoinkPaste_P)

let g:yoinkIncludeDeleteOperations = 1
