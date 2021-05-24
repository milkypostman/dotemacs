;;; custom.el --- customizations                     -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-mode t)
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
 '(backup-directory-alist '(("." . "~/.emacs.d/backups/")))
 '(backward-delete-char-untabify-method nil)
 '(blink-cursor-mode t)
 '(coffee-tab-width 2 t)
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(custom-enabled-themes '(dark-tango))
 '(custom-safe-themes
   '("6f441c0e5d8199f08eb4b73e9c697710282bcae95e5925b7649ddfa8cea2e24c" "eef383086ae1f47f75cda814adfb9868a3dafa99b6eb67fdff780a52d326d468" "98522c31200ec2ee2c84ae3dddac94e69730650096c3f4f751be4beece0f6781" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" default))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(delete-auto-save-files nil)
 '(delete-selection-mode t)
 '(diff-switches "-u")
 '(dired-use-ls-dired nil)
 '(echo-keystrokes 0.1)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(enable-recursive-minibuffers t)
 '(erc-hide-list '("JOIN" "PART" "QUIT") t)
 '(evil-leader/leader ",")
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#444444")
 '(foreground-color "#cccccc")
 '(fringe-indicator-alist
   '((truncation left-arrow right-arrow)
     (continuation nil right-curly-arrow)
     (overlay-arrow . right-triangle)
     (up . up-arrow)
     (down . down-arrow)
     (top top-left-angle top-right-angle)
     (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
     (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
     (empty-line . empty-line)
     (unknown . question-mark)) t)
 '(fringe-mode '(4 . 0) nil (fringe))
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-prettify-symbols-mode t)
 '(global-undo-tree-mode t)
 '(gofmt-command "goimports")
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence t)
 '(guide-key/idle-delay 0.5)
 '(helm-M-x-fuzzy-match t)
 '(helm-always-two-windows nil)
 '(helm-apropos-fuzzy-match t)
 '(helm-autoresize-mode t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-file-cache-fuzzy-match t)
 '(helm-follow-mode-persistent t)
 '(helm-imenu-fuzzy-match t)
 '(helm-lisp-fuzzy-completion t)
 '(helm-locate-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-all-abbrevs try-complete-lisp-symbol-partially try-complete-lisp-symbol))
 '(ibuffer-expert t)
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-max-prospects 10)
 '(ido-ubiquitous-mode t)
 '(ido-use-virtual-buffers t)
 '(ido-vertical-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist
   '((vertical-scroll-bars)
     (left-fringe . 4)
     (right-fringe . 0)))
 '(ispell-extra-args '("--sug-mode=ultra"))
 '(ispell-list-command "list" t)
 '(ispell-program-name "/opt/homebrew/bin/aspell")
 '(ivy-magic-tilde nil)
 '(ivy-mode t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(line-spacing 0)
 '(linum-format " %7i ")
 '(locale-coding-system 'utf-8 t)
 '(menu-bar-mode (display-graphic-p))
 '(mode-line-inverse-video t t)
 '(mouse-wheel-scroll-amount '(0.01))
 '(mouse-yank-at-point t)
 '(mp-rad-packages
   '(expand-region multiple-cursors magit ace-jump-mode ido-ubiquitous ido-vertical-mode smex undo-tree helm))
 '(ns-command-modifier 'meta)
 '(ns-pop-up-frames nil)
 '(ns-tool-bar-display-mode 'both t)
 '(ns-tool-bar-size-mode 'regular t)
 '(overflow-newline-into-fringe t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(ace-window highlight-symbol ivy-xref yasnippet jsonnet-mode projectile string-inflection phi-search wgrep-ag company-flx company ace-jump-buffer avy evil-mc evil-multiedit evil-anzu diff-hl ivy-prescient flx counsel deft ivy which-key guide-key evil-space fish-mode use-package pabbrev go-mode ahungry-theme browse-kill-ring arduino-mode cider smartparens clojure-mode evil evil-commentary evil-ediff evil-leader evil-surround markdown-mode magit ag powerline undo-tree flycheck-package auctex scad-mode smex expand-region multiple-cursors))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 '(redisplay-dont-pause t t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((checkdoc-minor-mode . 1)
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp 'flycheck-mode)
             (flycheck-mode -1))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require 'package-build)))
           (package-build-minor-mode)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp 'flycheck-mode)
             (flycheck-mode -1))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require 'package-build)))
           (package-build-minor-mode))))
 '(save-place-file "~/.emacs.d/places")
 '(save-place-mode t nil (saveplace))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 5)
 '(scroll-margin 5)
 '(send-mail-function 'mailclient-send-it)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-paren-style 'mixed)
 '(sp-autoskip-closing-pair 'always)
 '(sp-base-key-bindings 'paredit)
 '(sp-hybrid-kill-entire-symbol nil)
 '(split-height-threshold nil)
 '(split-width-threshold 159)
 '(tab-width 4)
 '(time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")
 '(tool-bar-mode nil)
 '(tramp-remote-path
   '(tramp-default-remote-path tramp-own-remote-path "/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"))
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/")))
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator " • ")
 '(user-full-name "Donald Ephraim Curtis")
 '(user-mail-address "d@milkbox.net")
 '(vc-follow-symlinks t)
 '(virtualenv-root "/Users/dcurtis/.virtualenv/")
 '(visual-line-mode nil t)
 '(whitespace-style
   '(face tabs trailing lines-tail space-before-tab newline indentation empty space-after-tab tab-mark))
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-sentence-face ((t (:foreground "white"))) t))



;;; custom.el ends here
