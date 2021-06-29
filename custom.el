;;; custom.el --- customizations                     -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
 '(backup-directory-alist '(("." . "~/.emacs.d/backups/")))
 '(custom-enabled-themes '(tsdh-dark))
 '(custom-safe-themes
   '("80d5a22931c15756b00fb258b80c93b8bc5096bb698dadfb6155ef3550e1c8fb" default))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(delete-auto-save-files nil)
 '(evil-leader/leader ",")
 '(evil-undo-system 'undo-fu)
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-auto-revert-mode t)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(marginalia-align-offset 2)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(package-selected-packages
   '(marginalia rg consult magit undo-fu selectrum-prescient selectrum evil-leader use-package evil))
 '(prescient-filter-method '(literal regexp initialism fuzzy))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(selectrum-mode t)
 '(selectrum-prescient-mode t)
 '(show-paren-mode t)
 '(split-height-threshold nil)
 '(tool-bar-mode nil)
 '(user-full-name "Donald Ephraim Curtis")
 '(user-mail-address "d@milkbox.net")
 '(vc-follow-symlinks t)
 '(winner-mode t))




;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(selectrum-prescient-secondary-highlight ((t (:inherit selectrum-prescient-primary-highlight)))))
