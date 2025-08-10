--- personal nvim config
require("roip/config/options")
require("roip/config/keymaps")
require("roip/config/autocommands")
require("roip/config/toggles")

-- lsp
require("roip/lsp")
require("roip/colorscheme")

--- plugins
require("roip/plugins/tmux")
require("roip/plugins/git")
require("roip/plugins/treesitter")
require("roip/plugins/mason")

--- mini snacks
require("roip/plugins/mini")
require("roip/plugins/snacks")

-- completions
require("roip/plugins/completion")

-- ai
require("roip/plugins/copilot")

--- inspektdq
require("roip/config/inspekto")
-------------------------------------------------------------------------------
--- Neovim External Command Cheatsheet
---
--- These are common ways to interact with external programs from Neovim:
---
--- calling external tools
--- :!<program> - no stdin, stdout shown in command line
--- :r !<program> - no stdin, stdout after current line on buffer
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
--- motions
--- o in visual selection - to change to other side/corner
--- gv - reselect last visual selection
--- `[, `] - go to first, last char of previously changed or yanked text
---
--- folding
---
--- commandline window
--- q/, q: or ctrl-f from /, :
---
--- changelist
--- g; g,
--- jumplist :help jump-motions (' ` G / ? n N % ( ) [ ] { } :s :tag L M H and new files)
--- ctrl-o ctrl-i
---
--- help
--- C-] C-o - navigate forward and backwards on links
---
--- completions (ctrl-x shows possible options in cmd line ^ means ctrl)
--- ctrl-x ctrl-o - omni completion (used for lsp)
--- ctrl-x ctrl-f - file path completion
--- ctrl-x ctrl-l - line completion
--- Ctrl-x Ctrl-n - keyword completion from current buffers
--- ctrl-x ctrl-p - keyword completion from other buffers
--- ctrl-x ctrl-k - dictionary completion (empty by default)
--- ctrl-x ctrl-t - thesaurus completion (empty by default)
--- ctrl-n or ctrl-p - general purpose
---
--- plugins at :echo stdpath("data")

