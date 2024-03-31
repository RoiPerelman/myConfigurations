;; General configurations
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; make esc work like C-g
;; (set-face-attribute 'default nil :font "Fira Code" :height 280)

; line numbers
(global-display-line-numbers-mode t) ; show line numbers
(column-number-mode) ; show column as well as line number in bottom line
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar rp/default-font-size 180)
(defvar rp/default-variable-font-size 180)

;; make sure we start emacs fullscreen and maximized
(set-frame-parameter (selected-frame) 'fullscreen 'maximized) ;; sets initial frame
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; sets next frames

;; Set frame transparency
(defvar rp/frame-transparency '(100 . 90))
(set-frame-parameter (selected-frame) 'alpha rp/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,rp/frame-transparency))

;; set custom file - so things wont be added in this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; add mepla as package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; add use-package
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; theme
(use-package dracula-theme
  :init (load-theme 'dracula))

;; (use-package doom-themes
;;   :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; completions
(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init (savehist-mode))

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
