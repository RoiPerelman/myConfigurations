(deftheme rp "Roip custom theme")

;; (custom-theme-set-faces
;;  'rp
;;  '(default ((t (:background "#777777" :foreground "#ffffff"))))
;;  '(font-lock-comment-face ((t (:foreground "#6a9955"))))
;;  '(font-lock-keyword-face ((t (:foreground "#569cd6" :weight bold))))
;;  ;; Additional faces here
;;  )

(let ((my-theme-colors
       ;; Define your colors here
       '((comment       . "#6a9955")
         (keyword       . "#569cd6")
         (string        . "#ce9178")
         (function-name . "#DCDCAA")
         (variable      . "#9CDCFE"))))
  (custom-theme-set-variables
   'rp
   '(tree-sitter-hl-face:comment       ((t (:foreground ,(alist-get 'comment my-theme-colors)))))
   '(tree-sitter-hl-face:keyword       ((t (:foreground ,(alist-get 'keyword my-theme-colors)))))
   '(tree-sitter-hl-face:string        ((t (:foreground ,(alist-get 'string my-theme-colors)))))
   '(tree-sitter-hl-face:function-name ((t (:foreground ,(alist-get 'function-name my-theme-colors)))))
   '(tree-sitter-hl-face:variable      ((t (:foreground ,(alist-get 'variable my-theme-colors))))))
   )
(provide 'rp)  ;; Make sure this is correct
