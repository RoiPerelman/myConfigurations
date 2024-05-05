;;; ──────────────────────────── 'General' ────────────────────────────

;; Disable startup message
(setq inhibit-startup-screen t)
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
;; Set default font
;; (set-face-attribute 'default nil :family "FiraCode Nerd Font")

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
;;; ──────────────────────────── 'Themes' ───────────────────────────

(use-package spacemacs-theme
  :config
  (setq-default spacemacs-theme-comment-italic t)
  :init (load-theme 'spacemacs-dark))

(use-package dracula-theme)
;;   :init (load-theme 'dracula))

(use-package doom-themes)
;;   :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package highlight-indent-guides
  :ensure t
  :hook
  (python-ts-mode . highlight-indent-guides)
  :config
  (setq highlight-indent-guides-method 'character))

;;; ────────────────────────── 'Completions' ──────────────────────────

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; ───────────────────── 'Minibuffer-Completions' ────────────────────

;; TODO: is this package already in emacs? can I just do
;; (savehist-mode 1)
;; saves history of minibuffer
(use-package savehist
  :init (savehist-mode))

;; Mini buffer completions
(use-package vertico
  :init (vertico-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init (marginalia-mode))

(use-package embark
  :init (embark-mode))
;; embark
;; consult

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

;;; ────────────────────────────── 'LSP' ──────────────────────────────

;; eglot

;;; ────────────────────────────── 'Git' ──────────────────────────────

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
