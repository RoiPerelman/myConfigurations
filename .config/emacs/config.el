(require 'package)
;; to make sure we are up to date (package-refresh-contents)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package emacs
  :ensure nil
  :init
  (setq inhibit-startup-screen t)     ; Disable startup message
  (setq ring-bell-function 'ignore)	; Disable annoying noisy bell
  (scroll-bar-mode -1)			; Disable visible scrollbar
  (tool-bar-mode -1)			; Disable the toolbar
  (tooltip-mode -1)			; Disable tooltips
  (menu-bar-mode -1)			; Disable the menu bar
  (set-fringe-mode 5)			; Add some space to lines
  (global-hl-line-mode t)                     ; Highlight current line
  (global-auto-revert-mode t)             ; Automatically update buffers if file changes on disk
  (delete-selection-mode 1)               ; Automatically delete selected text without backspace
  (setq use-short-answers t)		; Use y/n instead of yes/no
  (global-display-fill-column-indicator-mode 1) ; add column indicator
  (set-face-background 'fill-column-indicator "red") ; add color to column indicator
  )

(use-package emacs
  :ensure nil
  :init
  (global-display-line-numbers-mode 1)	; Show line numbers
  (column-number-mode 1)                  ; Show column as well as line number in bottom line

  ;; Disable line numbers for some modes
  (dolist (mode '(
                org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                ))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  )

(use-package emacs
  :ensure nil
  :init
  ;; make sure we start emacs fullscreen and maximized
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)	; sets initial frame
  (add-to-list 'default-frame-alist '(fullscreen . maximized))    ; sets next frames
  )

(use-package emacs
  :ensure nil
  :init
  (setq create-lockfiles nil) ; remove lockfiles emacs creates with .#<name> next to the actual file.

  (setq custom-file (concat user-emacs-directory "custom.el")) ; set custom file - so things wont be added in this file
  (load custom-file :no-error-if-file-is-missing)

  ;; set backup directory (Use copying to avoid symlinks)
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  (setq auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "saves") t)))
  )

;; when installing new packages - do not pop confusing warnings
;; they are produced by the byte compiler
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(use-package emacs
  :ensure nil
  :init
  (when (eq system-type 'darwin)
    ;; Change meta from option to command key
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'none))

  ;; make it so starting emacs as an app, actually use the shell for env variables
  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns x))
    :ensure t
    :config
    (exec-path-from-shell-initialize))
  )

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(modify-syntax-entry ?- "w")

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Available since Emacs 29 (Use `dabbrev-ignored-buffer-regexps' on older Emacs)
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; update isearch functionality
(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq isearch-lazy-count t)	   ; adds a counter eg 4/34 to isearch
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
  ;; push isearch search to project-find-regexp (C-x p g)
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

(use-package saveplace
  :ensure nil  ; It's built-in, no need to install
  :defer 3
  :custom
  (save-place-file (expand-file-name ".save-place" user-emacs-directory))
  (save-place-forget-unreadable-files t)
  :init
  (save-place-mode 1))

(defun toggle-comment-on-line-or-region ()
  "Toggle comment on the current line or active region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(use-package emacs
  :ensure nil
  :init
  ;; Set up keybindings for config workflow
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make esc work like C-g
  (global-set-key (kbd "M-o") 'other-window)              ; `C-x o' is a 2 step key binding. `M-o' is much easier.
  (global-set-key (kbd "C-;") 'toggle-comment-on-line)
  (global-set-key (kbd "M-k") 'kill-current-buffer)

  ;; zoom in and out
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

  ;; config management
  (global-set-key (kbd "M-s M-r")
		  (lambda () (interactive) (load-file "~/.config/emacs/init.el")))
  (defun rp/search-config ()
    "Open Emacs configuration file."
    (interactive)
    (find-file "~/.config/emacs/config.org"))
  (global-set-key (kbd "M-s M-c")
		  (lambda () (interactive) (find-file "~/.config/emacs/config.org")))
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete whitespace just when a file is saved.

(use-package roip-lib
  :load-path "~/.config/emacs/roip/"
  :init
  (defvar roip/inspekto-sync-project-root "/home/roip/sinspekto/inspekto/")
  (defvar roip/inspekto-sync-target-root "roip@192.168.0.151:/home/roip/sinspekto/winspekto/")
  ;; (defvar roip/inspekto-sync-target-root "rp@rp-il.net.plm.eds.com:/home/rp/sinspekto/winspekto/")
  ;; enable inspekto-sync-mode only if in inspekto project
  :hook (find-file . roip/enable-inspekto-sync-if-in-project))

;; require manual installation nerd-icons-install-fonts
 (use-package nerd-icons :ensure t)
 (use-package nerd-icons-completion
   :ensure t
   :after marginalia
   :config
   (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
 (use-package nerd-icons-corfu
   :ensure t
   :after corfu
   :config
   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(let ((mono-spaced-font "Fira Code") ; "JetBrains Mono" "Iosevka"
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 180)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

  ;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  ;; (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
  ;; (set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)
  ;; (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; to see colors M-x modus-themes-list-colors-current
;; to see original palette C-h f Modus-vivendi-palette
;; to see character info under the point - M-x describe-char
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
  :config (load-theme 'modus-vivendi :no-confirm-loading))

(use-package treesit
  :ensure nil
  ;; basically does for example
  ;; (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  :config
  (setq treesit-font-lock-level 4)
  ;; add lsp sources to be downloaded
  (add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python"))
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (add-to-list 'treesit-language-source-alist '(html "https://github.com/tree-sitter/tree-sitter-html"))
  (add-to-list 'treesit-language-source-alist '(css "https://github.com/tree-sitter/tree-sitter-css"))
  (add-to-list 'treesit-language-source-alist '(elisp "https://github.com/Wilfred/tree-sitter-elisp"))
  (add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
  (add-to-list 'treesit-language-source-alist '(make "https://github.com/alemuller/tree-sitter-make"))
  (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src"))
  (add-to-list 'treesit-language-source-alist '(json "https://github.com/tree-sitter/tree-sitter-json"))
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
  (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  (add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c"))
  (add-to-list 'treesit-language-source-alist '(cpp "https://github.com/tree-sitter/tree-sitter-cpp"))
  (add-to-list 'treesit-language-source-alist '(cmake "https://github.com/uyha/tree-sitter-cmake"))
  (add-to-list 'treesit-language-source-alist '(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"))
  ;; until treesit has markdown-ts-mode I can use this.
  ;; It still doesn't highlight code blocks
  (use-package markdown-ts-mode
    :ensure t
    :mode ("\\.md\\'" . markdown-ts-mode)
    :defer 't
    :config
    (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
    (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
    )

  ;; download sources
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source))
      (treesit-install-language-grammar (car source))))

  ;; add mode and file associations
  (progn
    ;; file associations
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.cjs\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.Dockerfile\\'" . dockerfile-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.bb\\'" . bash-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.bbappend\\'" . bash-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.bbclass\\'" . bash-ts-mode))

    ;; mode associations
    ;; now make <lang>-mode use <lang>-ts-mode instead
    ;; files that would normally open in python-mode should open in python-ts-mode
    (add-to-list 'major-mode-remap-alist '(bash-mode . bash-ts-mode))
    (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
    (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    )
  )

(use-package reformatter :ensure t)

(use-package eglot-booster
    :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "main")
	:after eglot
	:config	(eglot-booster-mode))

;; add ruff linting with flymake
;; can add a hook anywhere (add-hook 'python-ts-mode-hook . (flymake-ruff-load))
(use-package eglot
  :config
  ;; Set up workspace configuration for eglot (Pyright and Python-specific settings)
  ;; TODO: doesn't work for me. Need to setup pyrightconfig
  (setq-default eglot-workspace-configuration
                `((:pyright . (:disableOrganizeImports t))
                  (:python . (:analysis (:typeCheckingMode  "off"))))))

(use-package flymake-ruff :ensure t)

;; config is not called here
(use-package python-ts-mode
  :hook (
  	 (python-ts-mode . eglot-ensure)
  	 (python-ts-mode . flymake-ruff-load)
  	 (eglot-managed-mode . (
  				lambda ()
  				(when (derived-mode-p 'python-mode 'python-ts-mode)
  				  (flymake-ruff-load)
  				  (flymake-start)))))
  :mode (("\\.py\\'" . python-ts-mode))
  :init
  (require 'reformatter)
  (defcustom ruff-command "ruff" "Ruff command to use for formatting." :type 'string :group 'ruff-format)
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
  (defun ruff-fix-isort-format-buffer ()
    "Runs all ruff reformatters: ruff-fix, ruff-isort, and ruff-format."
    (interactive)
    (call-interactively 'ruff-fix-buffer)
    (call-interactively 'ruff-isort-buffer)
    (call-interactively 'ruff-format-buffer))
  )

(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1)
  ;; Automatically restart LSP after activating new venv
  (add-hook 'pyvenv-post-activate-hooks #'(lambda () (call-interactively #'eglot-reconnect))))

;; add eslint linting with flymake
;; can add a hook anywhere (add-hook 'typescript-ts-mode-hook . (flymake-eslint-enable))
(use-package flymake-eslint
  :ensure t
  :config
  (setq flymake-eslint-prefer-json-diagnostics t)
  (setq flymake-eslint-executable "eslint_d"))

(use-package typescript-ts-mode
  :hook (
  	 (typescript-ts-mode . eglot-ensure)
  	 (typescript-ts-mode . flymake-eslint-enable)
  	 (tsx-ts-mode . eglot-ensure)
  	 (tsx-ts-mode . flymake-eslint-enable)
  	 (eglot-managed-mode . (
  				lambda ()
  				(when (derived-mode-p 'typescript-ts-mode 'tsx-ts-mode)
  				  (flymake-eslint-enable)
  				  (flymake-start)))))
  :mode (
   ("\\.ts\\'" . typescript-ts-mode) ("\\.js\\'" . typescript-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode) ("\\.jsx\\'" . tsx-ts-mode))
  :config
  (require 'reformatter)
  (defcustom eslint-command "eslint_d" "ESLint command to use for formatting." :type 'string :group 'eslint-fix)
  (reformatter-define eslint-fix
    :program eslint-command
    :args (list "--fix-to-stdout" "--no-warn-ignored" "--stdin" "--stdin-filename" (or (buffer-file-name) input file))
    :lighter " ESLintFix"
    :group 'eslint-fix))

(use-package rust-ts-mode
  :mode (("\\.rs\\'" . rust-ts-mode))
  :hook ((rust-ts-mode . eglot-ensure)))

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  ;; :custom
  ;; Optional customizations
  ;; (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  ;; (jtsx-enable-jsx-electric-closing-element t)
  ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
  ;; (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d n") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j d a") 'jtsx-delete-jsx-attribute)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package org
  :hook
  (org-mode . my/org-mode-setup)
  :config
  (defun my/org-mode-setup ()
    (setq fill-column 100)
    (auto-fill-mode 1)))

(use-package magit
  :ensure t
  :bind (
	 ("C-x g" . magit-status)
	 ("C-c g g" . magit-status)
	 ("C-c g B" . magit-blame-addition)
	 )
  )

;; adds gutter add, change, revert indication
;; adds hunk controls
;; 1. go to next prev hunk
;; 2. show hunk diff
;; 3. stage, revert hunk (no unstage hunk)
(use-package git-gutter
  :ensure t
  ;; Replace the single hook with global mode and ensure
  :init
  (global-git-gutter-mode +1)
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
   '(git-gutter:deleted-sign " ")))

(use-package git-gutter-fringe
  :ensure t
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
  :ensure t
  :bind (("C-c g b" . blamer-mode))
  :config
  (setq blamer-idle-time 0.05)
  (setq blamer-author-formatter "%s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter ": %s")
  (setq blamer-max-commit-message-length 100)
  (setq blamer-min-offset 70))

(use-package savehist :ensure nil :config (savehist-mode))
(use-package recentf :ensure nil :config (recentf-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  ;; make sure we use orderless everywhere by setting these
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  ;; for corfu - not to slow down the system. use more basic matching style
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal)))

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local completion-styles '(orderless-literal-only basic)
                          completion-category-overrides nil
                          completion-category-defaults nil))))

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (setq marginalia-align 'right)
  (marginalia-mode))

;; Gives enhanced completion functions we need to bind
;; Gives previews for current item
;; binds M-s as opposed to native C-s C-r
(use-package consult
  :ensure t
  :bind (
         ("M-s M-g" . consult-ripgrep)
         ("M-s M-G" . consult-grep)
         ("M-s M-f" . consult-fd)
         ("M-s M-F" . consult-find)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer)
         ("M-s M-o" . consult-outline)
         ("M-s M-i" . consult-imenu)
         ("M-s M-t" . consult-theme)
         ("M-s M-m" . consult-mark)
         ("M-s M-h" . consult-info))
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package consult-project-extra
  :ensure t
  :after consult
  :bind (("C-c p f" . consult-project-extra-find)))

;; adds actions for current item
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; adds embark actions to consult functions
(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :init  ; Move initialization to :init instead of :config
  (global-corfu-mode 1)  ; Changed from global-corfu-mode to explicitly use 1
  (corfu-popupinfo-mode 1)
  :bind (:map corfu-map ("C-y" . corfu-complete))
  :custom
  (corfu-cycle t)                       ; Allows cycling through candidates
  (corfu-auto t)                        ; Enable auto completion
  (corfu-auto-prefix 2)                 ; Minimum length of prefix for completion
  (corfu-auto-delay 0.1)               ; delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2)) ; Automatically update info popup
  (corfu-preview-current nil)           ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)           ; Don't auto expand tempel snippets
  (corfu-min-width 20)
  :config
  ;; add minibuffer support but not while vertico is running
  (setq global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p mct--active)
                   (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map)))))

  ;; eshell and shell support
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode 1)))  ; Added explicit 1

  ;; Sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  ;; eshell specific settings
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-quit-at-boundary t
                         corfu-quit-no-match t
                         corfu-auto nil)
              (corfu-mode 1))  ; Added explicit 1
            nil
            t)

  ;; Move to minibuffer function
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))

  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  ;; Orderless optimization
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local completion-styles '(orderless-fast basic)
                          completion-category-overrides nil
                          completion-category-defaults nil))))

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

;; edit the results of a grep search  while inside a `grep-mode' buffer.
;; toggle editable mode, make changes, type C-c C-c to confirm | C-c C-k to abort.
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
            ("C-c C-c" . vterm--self-insert)))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(electric-indent-mode -1)
(require 'org-tempo)

 (add-hook 'org-mode-hook 'org-indent-mode)
 (use-package org-bullets
   :ensure t
   :config
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
   )

(use-package sudo-edit
  :ensure t
  :config
  (global-set-key (kbd "C-c f u") #'sudo-edit-find-file)
  (global-set-key (kbd "C-c f U") #'sudo-edit))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))))

;; adds colors to delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode 1))

;; adds colors to color indications e.g #fff000
(use-package rainbow-mode :ensure t)

;; M-x copilot-install-server
;; M-x copilot-login
(use-package copilot
  :ensure nil
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :branch "main")
  :init
  (use-package editorconfig :ensure t)
  (use-package f :ensure t)
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("M-<right>" . copilot-accept-completion)
              ("C-<right>" . copilot-accept-completion-by-word)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion))
  :config
  (setq copilot-max-char -1)
  )

(use-package keycast
  :ensure t
  :config
  ;; Show keys in the mode line
  (setq keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-mode t)

  ;; Optional: customize appearance

  ;; (setq keycast-mode-line-insert-after 'mode-line-misc-info)
  ;; (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)

  ;; Enable it globally
  (keycast-mode))
