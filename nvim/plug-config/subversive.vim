" s for substitute (siw subsitute inside word as an example)
nmap s <plug>(SubversiveSubstitute)
nmap ss <plug>(SubversiveSubstituteLine)
nmap S <plug>(SubversiveSubstituteToEndOfLine)

" search and replace (leader siwib)
nmap <leader>s <plug>(SubversiveSubstituteRange)
xmap <leader>s <plug>(SubversiveSubstituteRange)
nmap <leader>ss <plug>(SubversiveSubstituteWordRange)
nmap <leader>cs <plug>(SubversiveSubstituteRangeConfirm)
xmap <leader>cs <plug>(SubversiveSubstituteRangeConfirm)
nmap <leader>css <plug>(SubversiveSubstituteWordRangeConfirm)

" ie = inner entire buffer
onoremap ie :exec "normal! ggVG"<cr>

xmap s <plug>(SubversiveSubstitute)
xmap p <plug>(SubversiveSubstitute)
xmap P <plug>(SubversiveSubstitute)
