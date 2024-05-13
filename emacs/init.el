;;; ──────────────────────────── 'General' ────────────────────────────

;; Disable startup message
(setq inhibit-startup-screen t)
;; annoying noisy bell turned into annoying visible bell
(setq visible-bell 1)
;; Disable visible scrollbar
(scroll-bar-mode -1)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable tooltips
(tooltip-mode -1)
;; Disable the menu bar
(menu-bar-mode -1)
;; Add some space to lines
(set-fringe-mode 10)
;; Highlight current line
(global-hl-line-mode t)
;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)
;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;; adds a counter eg 4/34 to isearch
(setq isearch-lazy-count t)
;; change isearch space to not be literal but a non greedy regex
(setq search-whitespace-regexp "*.?")

;;; ─────────────────── 'General-Frame-Management' ──────────────────

;; make sure we start emacs fullscreen and maximized
;; sets initial frame
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; sets next frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set frame transparency
;; (defvar rp/frame-transparency '(100 . 90))
;; (set-frame-parameter (selected-frame) 'alpha rp/frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,rp/frame-transparency))

;;; ───────────────────── 'General-Line-Numbers' ────────────────────

;; Show line numbers
(global-display-line-numbers-mode 1)
;; Show column as well as line number in bottom line
(column-number-mode 1)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; ────────────────────────── 'General-Mac' ──────────────────────────

;; Change meta from option to command key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;; ──────────────────── 'General-File-Management' ────────────────────

;; set custom file - so things wont be added in this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; ─────────────────────── 'General-Functions' ───────────────────────

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun comment-pretty ()
  "Insert a comment with '─' (C-x 8 RET BOX DRAWINGS LIGHT HORIZONTAL) on each side."
  (interactive)
  (let* ((comment-char "─")
         (comment (read-from-minibuffer "Comment: "))
         (comment-length (length comment))
         (current-column-pos (current-column))
         (space-on-each-side (/ (- fill-column
                                   current-column-pos
                                   comment-length
                                   (length comment-start)
                                   ;; Single space on each side of comment
                                   (if (> comment-length 0) 2 0)
                                   ;; Single space after comment syntax sting
                                   1)
                                2)))
    (if (< space-on-each-side 2)
        (message "Comment string is too big to fit in one line")
      (progn
        (insert comment-start)
        (when (equal comment-start ";")
          (insert comment-start))
        (insert " ")
        (dotimes (_ space-on-each-side) (insert comment-char))
        (when (> comment-length 0) (insert " "))
        (insert comment)
        (when (> comment-length 0) (insert " "))
        (dotimes (_ (if (= (% comment-length 2) 0)
                      (- space-on-each-side 1)
                      space-on-each-side))
          (insert comment-char))))))

;;; ─────────────────────── 'General-Keybinding' ──────────────────────

;; Make esc work like C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; `C-x o' is a 2 step key binding. `M-o' is much easier.
(global-set-key (kbd "M-o") 'other-window)

;; use general function toggle comment on line
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;;; ───────────────────────── 'General-Hooks' ─────────────────────────

;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; ─────────────────────────── 'Packages' ──────────────────────────

;; add mepla as package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure all packages are downloaded automatically
(package-refresh-contents)
(setq use-package-always-ensure t)

;;; ────────────────────────────── 'UI' ─────────────────────────────
(use-package all-the-icons)

(use-package spacemacs-theme)
(use-package dracula-theme)
(use-package doom-themes)

;; Set default font
;; (set-face-attribute 'default nil :family "Iosevka" :weight 'light :height 130)
(set-face-attribute 'fixed-pitch nil :family "Victor Mono" :weight 'normal :height 120)
(set-face-attribute 'variable-pitch nil :family "Victor Mono" :weight 'normal :height 120)
(set-face-attribute 'default nil :family "Victor Mono" :weight 'normal :height 120)

;; to see colors M-x modus-themes-list-colors-current
;; to see original palette C-h f Modus-vivendi-palette
(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-variable-pitch t)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-prompts '(bold italic))
  ;; to override the palette
  (setq modus-vivendi-palette-overrides
	'(
	  ;; (comment red-intense)
	  ))
  :config (load-theme 'modus-vivendi))


(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; (use-package highlight-indent-guides
;;   :ensure t
;;   :hook
;;   (python-ts-mode . highlight-indent-guides)
;;   :config
;;   (setq highlight-indent-guides-method 'character))

;;; ────────────────────────── 'Completions' ──────────────────────────

;; Adds out-of-order pattern matching algorithm.
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

;;; ───────────────────── 'Minibuffer-Completions' ────────────────────

;; save minibuffer histories. Vertico uses to put recently selected options at the top.
(savehist-mode 1)
;; save recently visited files. Consult uses it to put recent files options at the top.
(recentf-mode 1)
;; Minibuffer live ui
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (vertico-mode))

;; Adds item annotations
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Gives enhanced completion functions we need to bind
;; Gives previews for current item
;; binds M-s as opposed to native C-s C-r
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-ripgrep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

;; adds actions for current item
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; adds embark actions to consult functions
(use-package embark-consult
  :ensure t)

;; edit the results of a grep search  while inside a `grep-mode' buffer.
;; toggle editable mode, make changes, type C-c C-c to confirm | C-c C-k to abort.
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;; ───────────────────── 'In-Buffer-Completions' ─────────────────────

;; corfu
;; capt

;; temporary - should be changed to corfu and capt
(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

;;; ────────────────────────── 'Treesitter' ─────────────────────────

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ───────────────────────────── 'Code' ────────────────────────────

;; automatically load elgot when working on certain languages
(use-package eglot
  :ensure t
  :hook ((python-base-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               `(python-base-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio"))))))

;; add linting
(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

;; in case I need to work on a python environment
;; works with venv and workon
(use-package pyvenv)

;; add formatter wip
(use-package reformatter
  :ensure t
  :config
  (defcustom ruff-format-command "ruff"
    "Ruff command to use for formatting."
    :type 'string
    :group 'ruff-format)
  (reformatter-define ruff-format
    :program ruff-format-command
    :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file))
    :lighter " RuffFmt"
    :group 'ruff-format))
;;; ────────────────────────────── 'Git' ──────────────────────────────

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;; ─────────────────────────────── Lua ───────────────────────────────

(use-package lua-mode
  :ensure t
  :config
  (lua-mode))
