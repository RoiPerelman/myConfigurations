(require 'package)
;; to make sure we are up to date (package-refresh-contents)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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

;; (set-face-attribute 'variable-pitch nil
;; 		    :family "Ubuntu"
;; 		    :weight 'semi-bold
;; 		    :height 120)
;; (set-face-attribute 'fixed-pitch nil
;; 		    :family "Jetbrains Mono"
;; 		    :weight 'normal
;; 		    :height 100)
;; (set-face-attribute 'default nil
;; 		    :family "Jetbrains Mono"
;; 		    :weight 'normal
;; 		    :height 110)
;; ;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono 14"))
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
  :config (load-theme 'modus-vivendi))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(use-package emacs
  :init
  ;; Set up keybindings for config workflow
  ;; Make esc work like C-g
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; `C-x o' is a 2 step key binding. `M-o' is much easier.
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "C-;") 'toggle-comment-on-line)
  (global-set-key (kbd "M-k") 'kill-current-buffer)

  ;; zoom in and out
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

  (global-set-key (kbd "M-s M-r")
    (lambda () (interactive) (load-file "~/.config/emacs/init.el")))
  (global-set-key (kbd "M-s M-c")
    (lambda () (interactive) (find-file "~/.config/emacs/config.org")))
  )

;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
  (setq marginalia-align 'right)
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
