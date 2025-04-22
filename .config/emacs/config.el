(use-package emacs
  :init
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
  (when (eq system-type 'darwin)
    ;; Change meta from option to command key
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'none))

  ;; make sure we start emacs fullscreen and maximized
  ;; sets initial frame
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  ;; sets next frames
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

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
)

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
  :config (load-theme 'modus-vivendi))

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
  :after vertico
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  ;; (setq marginalia-align 'right)
  (marginalia-mode))
