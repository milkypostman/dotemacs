;; color theming
;; (autoload 'color-theme-initialize "color-theme")
(require 'color-theme)
(defun color-theme-undo ()
  (interactive)
  ;; (color-theme-reset-faces)
  (color-theme-snapshot))

;; backup current color theme
(fset 'color-theme-snapshot (color-theme-make-snapshot))


;; (color-theme-initialize)
;; (require 'zenburn)
;; (color-theme-zenburn)
;; (require 'color-theme-hober2)
;; (color-theme-hober2)
;; (require 'color-theme-twilight)
;; (color-theme-twilight)
;; (require 'color-theme-inkpot)
;; (color-theme-inkpot)
;; (set-face-attribute 'hl-line nil
;; 		    :inherit 'unspecified
;; 		    :background "gray8")
;; (set-face-foreground 'hl-line nil)
;; (set-face-background 'hl-line nil)
;; (require 'color-theme-ir-black)
;; (color-theme-ir-black)

;; global hl mode doesn't look good with hober!
(global-hl-line-mode 1)
(show-paren-mode 1)


(if (not (window-system))
    (menu-bar-mode 0)
  (load "~/.emacs.d/milk/fringemark.el")
  )


