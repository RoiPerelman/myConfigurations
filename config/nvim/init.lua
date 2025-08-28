--- personal nvim config
require("roip/config/options")
require("roip/config/keymaps")
require("roip/config/autocommands")
require("roip/config/toggles")
require("roip/colorscheme")

--- plugins
require("roip/plugins/tmux")
require("roip/plugins/git")
require("roip/plugins/treesitter")
require("roip/plugins/mason")
require("roip/plugins/mini")
require("roip/plugins/oil")
require("roip/plugins/snacks")
require("roip/plugins/completion")
require("roip/lsp") -- after completion as it needs blink to add capablilities
require("roip/plugins/format")
require("roip/plugins/lint")
require("roip/plugins/copilot")

--- inspekto
require("roip/config/inspekto")

--- Neovim External Command Cheatsheet
--
-- These are common ways to interact with external programs from Neovim:
--
-- calling external tools
-- :!<program> - no stdin, stdout shown in command line
-- :r !<program> - no stdin, stdout after current line on buffer
-- :[<range>]!<program> - stdin, replaces the passed lines on buffer
-- :[<range>]w !<program> - stdin, stdout shown in command line
--
-- buffers
-- :ls - show buffers (% current window, # alternate, a active, h hidden)
-- ctrl-6 - switch to alternate buffer
-- ]b,]b to move between buffers
-- :e! - reload buffer from disk
-- :bd! - close current buffer without saving
--
-- windows
-- ctrl-w r - rotate
-- ctrl-w _ | - maximize
--
-- motions
-- o in visual selection - to change to other side/corner
-- gv - reselect last visual selection
-- `[, `] - go to first, last char of previously changed or yanked text
--
-- folding
-- zi - toggle fold enable/disable
-- zR, zM - open/close all folds
-- za, zA - toggle current fold (captial for recursive)
-- zj, zk - move up/down to top/bottom of next/previous fold
-- zo, zO, zc, zC - open/close current fold (captial for recursive)
-- zr, zm - increase/reduce fold level by 1
-- zv - expand the fold of the current position
-- zMzv - close all folds and expand the current one

-- commandline window
-- q/, q: or ctrl-f from /, :
--
-- changelist
-- g; g,
-- jumplist :help jump-motions (' ` G / ? n N % ( ) [ ] { } :s :tag L M H and new files)
-- ctrl-o ctrl-i
--
-- arglist
-- :args - show arglist
-- :next, :previous, :first, :last - move to next/previous/first/last arg
-- [a,]a - move to next/previous arg in arglist
-- :args <file> - add file to arglist
-- :argdo <command> - run command on all args in arglist
-- example
-- :silent argdo %s/\a/*/ge
-- replace all 'a' with '*' in all files in arglist. global and surpress error messages
-- :silent argdo edit! - revert like git reset --hard .
--
-- help
-- C-] C-o - navigate forward and backwards on links
-- help s_flags - show subsitute %s flags
-- help registers
-- help function-list, help system (for functions with expression register)
--
-- completions (ctrl-x shows possible options in cmd line ^ means ctrl)
-- ctrl-x ctrl-o - omni completion (used for lsp)
-- ctrl-x ctrl-f - file path completion
-- ctrl-x ctrl-l - line completion
-- Ctrl-x Ctrl-n - keyword completion from current buffers
-- ctrl-x ctrl-p - keyword completion from other buffers
-- ctrl-x ctrl-k - dictionary completion (empty by default)
-- ctrl-x ctrl-t - thesaurus completion (empty by default)
-- ctrl-n or ctrl-p - general purpose
--
-- plugins at :echo stdpath("data")
--
-- registers
-- used with :reg to show registers
-- used with " in normal mode to paste from a register
-- used with C-r in insert mode to paste from a register
-- :yank <register> - yank to register
-- " - default register
-- 0 - yank register
-- / - last search pattern
-- : - last command line
-- . - last inserted text
-- %, # - current, alternate file name
-- +, * - system clipboard, x11 primary selection
-- a-z - named registers
-- A-Z - append to named registers
-- = or :put = - expression register
-- exmaple :put = system('echo $RANDOM')
-- the example can also be done with calling external tools :r!

-- commands
-- :9copy18 - copy line 9 below line 18
-- :-9t. - t is a synonym for copy, . is current line
-- gn - select next search match. example usage cgn "whatever" . .
