(deftheme molokai
  "Created 2011-12-30.")

(custom-theme-set-faces
 'molokai
 '(cursor ((t (:background "#f8f8f0"))))
 '(font-lock-builtin-face ((t (:foreground "#ae81ff"))))
 '(font-lock-function-name-face ((t (:foreground "#a6e22e"))))
 '(font-lock-type-face ((t (:foreground "#66d9ef"))))
 '(font-lock-warning-face ((t (:inherit error))))
 '(error ((t (:background "#1e0010" :foreground "#960050"))))
 '(font-lock-keyword-face ((t (:foreground "#kf92672"))))
 '(font-lock-constant-face ((t (:foreground "#fd971f"))))
 '(font-lock-string-face ((t (:foreground "#e6db74"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#75715e"))))
 '(show-paren-match ((t (:background "#fd971f" :foreground "#000000"))))
 '(esk-paren-face ((t (:foreground "grey50"))))
 '(whitespace-tab ((t (:background "#272822"))))
 '(mode-line ((t (:background "#808080" :foreground "#000000" :box nil))))
 '(mode-line-inactive ((t (:box nil :foreground "#75715e" :background "#080808" :inherit mode-line))))
 '(hl-line ((t (:inherit highlight))))
 '(highlight ((t (:background "#49483e"))))
 '(region ((t (:inherit highlight))))
 '(default ((t (:background "#1b1d1e" :foreground "#f8f8f2")))))

(provide-theme 'molokai)
