(deftheme ir_black
  "The last emacs24 theme you'll ever need.")

(custom-theme-set-faces
 'ir_black
 '(cursor ((t (:background "#ffa560"))))
 '(mode-line ((t (:background "#202020" :foreground "#9c9c9c" :box nil :slant italic :height 0.9))))
 '(mode-line-inactive ((t (:background "#202020" :foreground "#4a4a4a" :box nil :slant normal))))
 '(font-lock-builtin-face ((t (:foreground "#96cbfe"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground "#e9c062"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground "#e9c062"))))
 '(font-lock-comment-face ((t (:foreground "#7c7c7c"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-keyword-face ((t (:foreground "#96cbfe"))))
 '(font-lock-function-name-face ((t (:foreground "#ffd2a7"))))
 '(font-lock-type-face ((t (:foreground "#ffffb6"))))
 '(font-lock-string-face ((t (:foreground "#a8ff60"))))
 '(escape-glyph ((t (:foreground "#00a0a0"))))
 '(font-lock-variable-name-face ((t (:foreground "#c6c5fe"))))
 '(font-lock-constant-face ((t (:foreground "#99cc99"))))
 '(esk-paren-face ((t (:foreground "#00a0a0"))))
 '(font-lock-number-face ((t (:inherit font-lock-constant-face :foreground "#ff73fd"))))
 '(default ((t (:background "black" :foreground "#f6f3e8")))))

(provide-theme 'ir_black)
