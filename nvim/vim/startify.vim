let g:startify_session_dir = '~/.config/nvim/session'

" returns all modified files of the current git repo
" `2>/dev/null` makes the command fail quietly, so that when we are not
" in a git repo, the list will be empty
function! s:gitModified()
  let files = systemlist('git ls-files -m 2>/dev/null')
  return map(files, "{'line': v:val, 'path': v:val}")
endfunction


let g:startify_session_autoload = 1
let g:startify_session_delete_buffers = 1
let g:startify_change_to_vcs_root = 1
let g:startify_fortune_use_unicode = 1
let g:startify_session_persistence = 1
let g:startify_enable_special = 0

let g:startify_lists = [
      \ { 'type': 'bookmarks', 'header': ['   Bookmarks']                    },
      \ { 'type': 'sessions',  'header': ['   Sessions']                     },
      \ { 'type': function('s:gitModified'),  'header': ['   git modified']  },
      \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
      \ { 'type': 'files',     'header': ['   Files']                        },
      \ { 'type': 'commands',  'header': ['   Commands']                     },
      \ ]

let g:startify_bookmarks = [
      \ { 'm': '~/myConfigurations' },
      \ { 'c': '~/tiny_inspektor/sw/fixi_client' },
      \ ]
      " \ { 't': '~/tiny_inspektor' },
      " \ { 'f': '~/tiny_inspektor/sw/fixi' },
      " \ { 'db': '~/tiny_inspektor/sw/tiny_database' },
      " \ { 'dc': '~/tiny_inspektor/sw/data_coordinator' },
      " \ { 's': '~/tiny_inspektor/sw/tier2/tiny_std' },

