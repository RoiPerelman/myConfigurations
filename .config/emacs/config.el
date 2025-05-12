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

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode 1))

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
  (global-set-key (kbd "M-s M-c")
    (lambda () (interactive) (find-file "~/.config/emacs/config.org")))
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete whitespace just when a file is saved.

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

(let ((mono-spaced-font "Monospace")
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
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.lua\\'" . lua-ts-mode)
	 ;; BitBake files
         ("\\.bb\\'" . bash-ts-mode)
         ("\\.bbappend\\'" . bash-ts-mode)
         ("\\.bbclass\\'" . bash-ts-mode)
         ("\\.inc\\'" . bash-ts-mode))

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
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source))
      (treesit-install-language-grammar (car source))))

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

(use-package combobulate
    :ensure nil
    :vc (:url "https://github.com/mickeynp/combobulate")
;;    :load-path "~/.config/emacs/combobulate"
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode)))

(use-package reformatter :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; (lsp-prefer-flymake t) ;; We prefer flymake if available
  (lsp-diagnostics-provider :flycheck)
  (lsp-diagnostic-package :flycheck)
  (lsp-enable-snippet nil) ;; Optional: disable snippets
  (lsp-completion-provider :none) ;; stop using company as #'completion-at-point
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-log-io nil) ;; Debug: can set to t if you want to debug LSP issues
  :init
  (setq lsp-use-plists t)
  ;; https://github.com/blahgeek/emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
	 (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
			 (fboundp 'json-parse-buffer))
                  'json-parse-buffer
		'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
	orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package flycheck
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . flycheck-mode))

(use-package consult-flycheck :ensure t :after consult)

;; Pyright LSP setup. Needs require 'lsp-pyright somewhere before loading lsp
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :custom
  (lsp-pyright-type-checking-mode "off") ;; or "basic" / "strict"
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-disable-organize-imports t))

;; Python major mode
(use-package python-ts-mode
  :hook ((python-ts-mode . (lambda()
  			     (require 'lsp-pyright)
  			     ;; we need for another package as its already included in lsp-mode
  			     (require 'lsp-ruff)
  			     (lsp-deferred))))
  :mode (("\\.py\\'" . python-ts-mode)))

;; Pyvenv for managing Python virtualenvs
(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode 1)
  ;; Automatically restart LSP after activating new venv
  (add-hook 'pyvenv-post-activate-hooks (lambda () (when (bound-and-true-p lsp-mode) (lsp-restart-workspace)))))

(use-package lsp-eslint
  :demand t
  :after lsp-mode
  :init
  (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  :config
  (require 'lsp-eslint))

;; Python major mode
(use-package typescript-ts-mode
  :hook (((tsx-ts-mode typescript-ts-mode js-ts-mode) . lsp-deferred))
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

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
  :config
  (setq completion-styles '(orderless basic))
  ;; make sure we use orderless everywhere by setting these to nil
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

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
  :bind (:map corfu-map ("C-y" . corfu-complete))
  :custom
  (corfu-cycle t)                       ; Allows cycling through candidates
  (corfu-auto t)                        ; Enable auto completion
  (corfu-auto-prefix 2)                 ; Minimum length of prefix for completion
  (corfu-auto-delay 0)                  ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current nil)           ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)            ; Don't auto expand tempel snippets
  (corfu-min-width 20)
  :config
  (global-corful-mode)
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'
  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  ;; TODO: what is that - do i want it?
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

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
  (use-package dash :ensure t)
  (use-package s :ensure t)
  (use-package editorconfig :ensure t)
  (use-package f :ensure t)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion))
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-max-char -1)
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )
