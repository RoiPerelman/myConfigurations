;;; ────────────────────────────── 'General' ─────────────────────────────

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
;; Set default font
(set-face-attribute 'default nil :font "FiraCode Nerd Font")

;;; ────────────────────────────── 'General-Frame-Management' ─────────────────────────────
;; make sure we start emacs fullscreen and maximized
;; sets initial frame
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; sets next frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set frame transparency
;; (defvar rp/frame-transparency '(100 . 90))
;; (set-frame-parameter (selected-frame) 'alpha rp/frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,rp/frame-transparency))

;;; ────────────────────────────── 'General-Line-Numbers' ─────────────────────────────
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

;;; ────────────────────────────── 'GENERAL-Mac' ─────────────────────────────
;; Change meta from option to command key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;; ────────────────────────────── 'General-File-Management' ─────────────────────────────

;; set custom file - so things wont be added in this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; ────────────────────────────── 'General-Keybidings' ─────────────────────────────

;; Make esc work like C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; ────────────────────────────── 'Packages' ─────────────────────────────

;; add mepla as package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure all packages are downloaded automatically
(setq use-package-always-ensure t)

;;; ────────────────────────────── 'Themes' ─────────────────────────────

(use-package spacemacs-theme
  :config
  (setq spacemacs-theme-comment-italic t)
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

;;; ────────────────────────────── 'Minibuffer' ─────────────────────────────

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

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
