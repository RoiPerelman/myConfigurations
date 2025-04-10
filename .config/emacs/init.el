;;; ──────────────────────────── 'General' ────────────────────────────
;; Disable startup message
(setq inhibit-startup-screen t)
;; Disable annoying noisy bell
(setq ring-bell-function 'ignore)
;; Disable visible scrollbar
(scroll-bar-mode -1)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable tooltips
(tooltip-mode -1)
;; Disable the menu bar
(menu-bar-mode -1)
;; Add some space to lines
(set-fringe-mode 5)
;; Highlight current line
(global-hl-line-mode t)
;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)
;; automatically delete selected text without backspace
(delete-selection-mode 1)
;; Change all yes/no questions to y/n type (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; for dired
(setq dired-dwim-target t)
;;; ───────────────────── 'General-Line-Numbers' ────────────────────
;; Show line numbers
(global-display-line-numbers-mode 1)
;; Show column as well as line number in bottom line
(column-number-mode 1)
;; Disable line numbers for some modes
(dolist (mode '(
		org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		vterm-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; ────────────────────────── 'General-Mac' ──────────────────────────
;; Change meta from option to command key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;; ─────────────────── 'General-Frame-Management' ──────────────────
;; make sure we start emacs fullscreen and maximized
;; sets initial frame
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; sets next frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; ───────────────────────── 'General-Fonts' ─────────────────────────
;; Set default font
;; (set-face-attribute 'default nil :family "Iosevka" :weight 'normal :height 150)
;; (set-face-attribute 'default nil :family "Cartograph CF" :weight 'normal :height 110)
;; (set-face-attribute 'default nil :family "SauceCodePro NF" :weight 'normal :height 100)
(set-face-attribute 'variable-pitch nil
		    :family "Cartograph CF"
		    :weight 'semi-bold
		    :height 120)
(set-face-attribute 'fixed-pitch nil
		    :family "FiraCode Nerd Font"
		    :weight 'normal
		    :height 100)
(set-face-attribute 'default nil
		    :family "Cascadia Code NF"
		    :weight 'normal
		    :height 110)
;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono 14"))
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;;; ──────────────────── 'General-File-Management' ────────────────────
;;; ───────────────────────── 'No-littering' ────────────────────────
;; set custom file - so things wont be added in this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; set backup directory (Use copying to avoid symlinks)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "saves") t)))

;;; ─────────────────────── 'General-packages' ──────────────────────
;; add mepla as package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)
;; install use package if necessary
(unless (package-installed-p 'use-package) (package-install 'use-package))
;; Ensure all packages are downloaded automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(require 'use-package)
;; to make sure we are up to date (package-refresh-contents)

;; can be removed in emacs 30+
;; we package-vc-install to install a github package that helps
;; install packages from github using use-pacakge easily
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

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

(global-set-key (kbd "M-k") 'kill-current-buffer)

;;; ───────────────────────── 'General-Hooks' ─────────────────────────
;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; ───────────────────────── 'flow-packages' ─────────────────────────
;; make us go (or delete) forward and backwards better
(use-package syntax-subword
  :config (global-syntax-subword-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))))

;; show search & replace on buffer
(use-package anzu
  :bind (:map isearch-mode-map
   ([remap isearch-query-replace]        . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))
;; missing feature M-n doesn't work to get thing at point, need to use anzu-query-replace-at-point[-thing]
;;   :config
;;   (global-set-key [remap query-replace] 'anzu-query-replace)
;;   (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; automatically insert corresponding closing parenthesis
(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode 1))

;; update isearch functionality
(use-package isearch
  :ensure nil
  :defer t
  :config
  ;; adds a counter eg 4/34 to isearch
  (setq isearch-lazy-count t)
  ;; change isearch space literal to non greedy regex (this acts weird, doesn't mark the whole area)
  ;; (setq search-whitespace-regexp "*.?")

  ;; use selection to search (https://www.reddit.com/r/emacs/comments/2amn1v/comment/cixq7zx/)
  (defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
    (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
        (progn
          (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
          (deactivate-mark)
          ad-do-it
          (if (not forward)
              (isearch-repeat-backward)
            (goto-char (mark))
            (isearch-repeat-forward)))
      ad-do-it))
  ;; push isearch search to porject-find-regexp (C-x p g)
  (defun isearch-rp-project ()
    (interactive)
    (let ((query (if isearch-regexp
		     isearch-string
		   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (project-find-regexp query)))
  (defun isearch-rp-consult-line ()
    "Invoke `consult-line' from isearch."
    (interactive)
    (let ((query (if isearch-regexp
		     isearch-string
		   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (consult-line query)))
  :bind
  (:map isearch-mode-map
	("M-o" . isearch-occur)
        ("M-p" . isearch-rp-project)
	("M-." . isearch-forward-thing-at-point)
	("M-l" . isearch-rp-consult-line)
	;; ("C-j" . avy-isearch)
	)
  )

;;; ──────────────────────── 'helper-packages' ────────────────────────
;; adds a menu that shows possible key presses
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; adds colors to delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode 1))
;; adds colors to color indications e.g #fff000
(use-package rainbow-mode)

;;; ────────────────────────────── 'UI' ─────────────────────────────
;; icons
(use-package all-the-icons)
(use-package all-the-icons-completion)
(use-package all-the-icons-dired)

;; themes
(use-package spacemacs-theme)
(use-package dracula-theme)
(use-package atom-one-dark-theme)

;; to see colors M-x modus-themes-list-colors-current
;; to see original palette C-h f Modus-vivendi-palette
;; to see character info under the point - M-x describe-char
(use-package modus-themes
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



;; (load-file "/Users/roiperelman/.config/emacs/rp-theme.el")
;; (load-theme 'rp t)

;;; ────────────────────────── 'Treesitter' ─────────────────────────
;; better way to parse code
(use-package treesit
  :ensure nil
  :config
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python"))
  (add-to-list 'treesit-language-source-alist
               '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (add-to-list 'treesit-language-source-alist
               '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (add-to-list 'treesit-language-source-alist
               '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (add-to-list 'treesit-language-source-alist
               '(html "https://github.com/tree-sitter/tree-sitter-html"))
  (add-to-list 'treesit-language-source-alist
               '(css "https://github.com/tree-sitter/tree-sitter-css"))
  (add-to-list 'treesit-language-source-alist
               '(elisp "https://github.com/Wilfred/tree-sitter-elisp"))
  (add-to-list 'treesit-language-source-alist
               '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
  (add-to-list 'treesit-language-source-alist
               '(make "https://github.com/alemuller/tree-sitter-make"))
  (add-to-list 'treesit-language-source-alist
               '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src"))
  (add-to-list 'treesit-language-source-alist
               '(json "https://github.com/tree-sitter/tree-sitter-json"))
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
  (add-to-list 'treesit-language-source-alist
               '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  (add-to-list 'treesit-language-source-alist
               '(cmake "https://github.com/uyha/tree-sitter-cmake"))
  (use-package markdown-ts-mode
    :mode (
	   ("\\.md\\'" . markdown-ts-mode)
	   )
    :defer 't
    :config
    (add-to-list 'treesit-language-source-alist
		 '(markdown
		   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
		   "split_parser"
		   "tree-sitter-markdown/src"))
    (add-to-list 'treesit-language-source-alist
		 '(markdown-inline
		   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
		   "split_parser"
		   "tree-sitter-markdown-inline/src")))
  (setq treesit-font-lock-level 4)
  (add-to-list 'auto-mode-alist '("\\.Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  ;; files that would normally open in python-mode should open in python-ts-mode
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(bash-mode . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(markdown-mode . markdown-ts-mode))
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source))
      (treesit-install-language-grammar (car source)))))

;;; ────────────────────────── 'Completions' ──────────────────────────

;; Adds out-of-order pattern matching algorithm.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))
;; (use-package orderless
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-defaults nil)
;;   (orderless-matching-styles
;;    '(orderless-literal
;;      orderless-prefixes
;;      orderless-initialism
;;      orderless-regexp
;;      )))
;;; ───────────────────── 'Minibuffer-Completions' ────────────────────

;; save minibuffer histories. Vertico uses to put recently selected options at the top.
(savehist-mode 1)
;; save recently visited files. Consult uses it to put recent files options at the top.
(recentf-mode 1)
;; Minibuffer live ui
(use-package vertico
  :config
  (setq vertico-cycle t)
  (vertico-mode))

;; Adds item annotations
(use-package marginalia
  :after vertico
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  ;; (setq marginalia-align 'right)
  (marginalia-mode))

;; Gives enhanced completion functions we need to bind
;; Gives previews for current item
;; binds M-s as opposed to native C-s C-r
(use-package consult
  :bind (;; A recursive grep
         ("M-s M-g" . consult-ripgrep)
	 ("M-s M-G" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-fd)
	 ("M-s M-F" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer/bookmarked/recent file.
         ("M-s M-b" . consult-buffer)
	 ;; search on imenu
	 ("M-s M-i" . consult-imenu)
	 ;; change theme
	 ("M-s M-t" . consult-theme)
	 ;; search mark
	 ("M-s M-m" . consult-mark)
	 ;; search help info
	 ("M-s M-h" . consult-info)
	 )
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args)))

  (defvar consult-rp-project-files-source
    (list :name     "Project Files"
          :category 'file
          :narrow   ?p
          :face     'consult-file
          :history  'file-name-history
          :state    #'consult--file-state
	  ;; :action   ,(lambda (file)
	  ;; 	       (consult--file-action
	  ;; 		(expand-file-name file (project-root (project-current)))))
          :items
	  ;; TODO: shorten the name and color me different
	  (lambda ()
	    (when-let* ((project (project-current))
			(root (project-root project)))
	      ;; (mapcar (lambda (file) (file-relative-name file root))
		      (project-files project)))))

  (add-to-list 'consult-buffer-sources 'consult-rp-project-files-source 'append)
  )

;; adds actions for current item
(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; adds embark actions to consult functions
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; edit the results of a grep search  while inside a `grep-mode' buffer.
;; toggle editable mode, make changes, type C-c C-c to confirm | C-c C-k to abort.
(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;; ───────────────────── 'In-Buffer-Completions' ─────────────────────

;; corfu popup buffer with choice options.
;; ATM done with vertico and consult consult-completion-in-region instead of completion--in-region

;; (use-package corfu
;;   :config
;;   (setq corfu-auto t)
;;   (setq corfu-auto-delay 0)
;;   (setq corfu-auto-prefix 0)
;;   (setq corfu-quit-no-match t)
;;   :init
;;   (global-corfu-mode))
;; cape adds capf sources (files, abbrev, etc)

;;; ───────────────────────────── 'Code' ────────────────────────────

;; new way - using vc option in use-package
;; old way instead of :vc we put but we need to download copilot manualy
;; :load-path "manual-packages/copilot.el"
;; mkdir ~/.config/emacs/manual-packages && cd ~/.config/emacs/manual-packages
;; git clone https://github.com/copilot-emacs/copilot.el
;; M-x copilot-install-server
;; M-x copilot-login
(use-package copilot
  :vc (copilot :url "https://github.com/copilot-emacs/copilot.el"
               :branch "main")
  :init
  ;; prerequisites
  (use-package dash)
  (use-package s)
  (use-package editorconfig)
  (use-package f)
  :bind (:map copilot-completion-map
	      ("<tab>" . copilot-accept-completion)
	      ("TAB" . copilot-accept-completion))
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-max-char -1)
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2))
  )

;; automatically load elgot when working on certain languages
(use-package eglot
  :hook (
	 (python-base-mode . eglot-ensure)
	 (typescript-ts-base-mode . eglot-ensure)
	 )
  ;; :config
  ;; add mode line indication for as [eglot: language-server-name]
  ;; has an issue atm - I can see double the [eglot: language-server-name]
  ;; (setq-default mode-line-format
  ;;             (append mode-line-format
  ;;                     '((:eval (when eglot--managed-mode
  ;;                                (let* ((server (eglot-current-server))
  ;;                                       (command (and server (process-command (jsonrpc--process server))))
  ;;                                       (name (and command (file-name-nondirectory (car command)))))
  ;;                                  (when name
  ;;                                    (format "[eglot: %s]"
  ;;                                            (replace-regexp-in-string
  ;;                                             "-\\(langserver\\|language-server\\)$" "" name)))))))))
  )

;; TODO: doesn't work - why?
;; working with python pyright and ruff
;; options:
;; 1. using pyright and ruff globally (preferable)
;; make sure each project has a pyrightconfig.json and in it we have venvPath and venv
;; eglot pyright will automatically get the right virtual environment
;; ruff will get the right config from pyproject.toml
;; 2. if ruff and pyright are not installed globally but only in a venv
;; use pyvenv - run M-x pyvenv-activate for venv and pyvenv-workon for virtualenv. after that eglot-reconnect

;; in case I need to work on a python environment - works with venv and workon
(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (call-interactively #'eglot-reconnect)))
  (pyvenv-mode +1))

;; add ruff linting with flymake
(use-package flymake-ruff
  :hook (eglot-managed-mode . flymake-ruff-load))

;; add ruff fix, isort and format
;; TODO: check how I can only add these functions during python-base-mode
(use-package reformatter
  :config
  (require 'reformatter)
  (defcustom ruff-command "ruff"
    "Ruff command to use for formatting."
    :type 'stringnn
    :group 'ruff-format)
  (reformatter-define ruff-fix
    :program ruff-command
    :args (list "check" "--fix" "--stdin-filename" (or (buffer-file-name) input-file))
    :lighter " RuffFix"
    :group 'ruff-format)
  (reformatter-define ruff-isort
    :program ruff-command
    :args (list "check" "--select=I" "--fix" "--stdin-filename" (or (buffer-file-name) input-file))
    :lighter " RuffIsort"
    :group 'ruff-format)
  (reformatter-define ruff-format
    :program ruff-command
    :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file))
    :lighter " RuffFmt"
    :group 'ruff-format)

  (defun ruff-fix-all-buffer ()
    "Runs all ruff reformatters: ruff-fix, ruff-isort, and ruff-format."
    (interactive)
    (call-interactively 'ruff-fix-buffer)
    (call-interactively 'ruff-isort-buffer)
    (call-interactively 'ruff-format-buffer))
  )

;;; ────────────────────────────── 'Git' ──────────────────────────────

(use-package magit
  :bind (
	 ("C-x g" . magit-status)
	 ("C-c g g" . magit-status)
	 ("C-c g B" . magit-blame-addition)
	 ))

;; adds gutter add, change, revert indication
;; adds hunk controls
;; 1. go to next prev hunk
;; 2. show hunk diff
;; 3. stage, revert hunk (no unstage hunk)
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :bind (
	 ("M-] h" . git-gutter:next-hunk)
	 ("M-[ h" . git-gutter:previous-hunk)
	 ("C-c h s" . git-gutter:stage-hunk)
	 ("C-c h r" . git-gutter:revert-hunk)
	 ("C-c h p" . git-gutter:popup-hunk)
	 )
  :config
  (setq git-gutter:update-interval 0.05)
  (custom-set-variables
   '(git-gutter:window-width 1)
   '(git-gutter:modified-sign " ") ;; two space
   '(git-gutter:added-sign " ")    ;; multiple character is OK
   '(git-gutter:deleted-sign " "))
  )

(use-package git-gutter-fringe
  :config
  (fringe-helper-define 'git-gutter-fr:added '(center repeated) ".")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated) ".")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom ".")
  )

;; for git blame there is
;; 1. magit-blame-addition (fast and adds lines on buffer) (C-c g B)
;; 2. vc-annotate (creates a new buffer with git blame on each line (C-x v g)
;; 3. blamer-mode which is a git line blame
(use-package blamer
  :bind (("C-c g b" . blamer-mode))
  :config
  (setq blamer-idle-time 0.05)
  (setq blamer-author-formatter "%s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter ": %s")
  (setq blamer-max-commit-message-length 100)
  (setq blamer-min-offset 70))

;; ediff
(use-package ediff
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  ;; add an option to copy both a and b to c. from https://stackoverflow.com/a/29757750/864684
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
		     (concat
		      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))
;; (setq ediff-diff-options "")
;; (setq ediff-custom-diff-options "-u")
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (setq ediff-split-window-function 'split-window-vertically)

;;; ─────────────────────────── 'Terminal' ──────────────────────────
(use-package vterm
  :bind (:map vterm-mode-map
	      ("C-c C-c" . vterm--self-insert)))

;; ;; TODO: check me out
;; (use-package vterm-toggle
;;   :requires vterm
;;   :config
;;   (setq vterm-min-window-width 30)
;;   (setq vterm-toggle-fullscreen-p nil)
;;   (add-to-list 'display-buffer-alist
;;              '((lambda (buffer-or-name _)
;;                    (let ((buffer (get-buffer buffer-or-name)))
;;                      (with-current-buffer buffer
;;                        (or (equal major-mode 'vterm-mode)
;;                            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;;                 (display-buffer-reuse-window display-buffer-at-bottom)
;;                 (reusable-frames . visible)
;;                 (window-height . 0.35))))

;;; ─────────────────────────── 'check-me' ──────────────────────────
;; check hl-todo
;; (use-package hl-todo
;;   :custom-face
;;   (hl-todo                        ((t (:inverse-video nil :italic t :bold nil))))
;;   :config
;;   (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
;;   (add-to-list 'hl-todo-keyword-faces '("WHY" . "#7cb8bb"))
;;   (global-hl-todo-mode +1))

;; check outline minor mode
;; understand imenu
;; add imenu list
