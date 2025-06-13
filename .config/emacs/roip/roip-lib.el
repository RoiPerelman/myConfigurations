;;; roip-lib.el --- Central loader for Roip's Emacs utilities -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all core roip/ functionality.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'roip-inspekto-sync)
;; (require 'roip-helpers)
;; (require 'roip-keybindings)

(provide 'roip-lib)
;;; roip-lib.el ends here
