-- personal nvim config
require("roip/config/options")
require("roip/config/keymaps")
require("roip/config/autocommands")

--- plugins
require("roip/plugins/tmux")
require("roip/plugins/git")
require("roip/plugins/treesitter")
require("roip/plugins/mason")

-- lsp
require("roip/lsp")
-------------------------------------------------------------------------------
--- Neovim External Command Cheatsheet
---
--- These are common ways to interact with external programs from Neovim:
---
--- calling external tools
--- :!<program> - no stding, stdout shown in command line
--- :r !<program> - no stding, stdout after current line on buffer
--- :[<range>]!<program> - stdin, replaces the passed lines on buffer
--- :[<range>]w !<program> - stdin, stdout shown in command line
---
--- buffers
--- :ls - show buffers (% current window, # alternate, a active, h hidden)
--- ctrl-6 - switch to alternate buffer
--- ]b,]b to move between buffers
--- :e! - reload buffer from disk
---
--- windows
--- ctrl-w r - rotate
--- ctrl-w _ | - maximize
---
--- changelist
--- g; g,
--- jumlist :help jump-motions (' ` G / ? n N % ( ) [ ] { } :s :tag L M H and new files)
--- ctrl-o ctrl-i
---
--- help
--- C-] C-o - navigate forward and backwards on links
---
--- completions (ctrl-x shows possible options in cmd line ^ means ctrl)
--- ctrl-x ctrl-o - omnicompletion (used for lsp)
--- ctrl-x ctrl-f - file path completion
--- ctrl-x ctrl-l - line completion
--- Ctrl-x Ctrl-n - keyword completion from current buffers
--- ctrl-x ctrl-p - keyword completion from other buffers
--- ctrl-x ctrl-k - dictionary completion (empty by default)
--- ctrl-x ctrl-t - thesaurus completion (empty by defualt)
--- ctrl-n or ctrl-p - general purpose
