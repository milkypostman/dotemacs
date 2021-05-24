;;; init.el --- Donald Config

;; Copyright (C) 2014 Donald Curtis
;; Author: Donald Curtis <d@milkbox.net>

;;; Commentary:
;;
;; This is my init. Kaboom!

;;; Code:

;;;; vendor
(add-to-list 'load-path "~/.emacs.d/vendor")
(let ((default-directory "~/.emacs.d/vendor"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/themes")

(defalias #'yes-or-no-p #'y-or-n-p)

;;;; package.el
(require 'package)
(setq package-enable-at-startup nil)
;; Version-specific package directory so that multiple versions can be
;; supported at the same time.
(setq package-user-dir
      (format "~/.emacs.d/elpa/%d.%d" emacs-major-version emacs-minor-version))
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;; initalize packages and make sure that use-package is installed
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package custom
  :config
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file))


;;; emacsclient-status-mode
(defvar emacsclient-status-need-die nil)

(defun emacsclient-status-kill-current-buffer ()
  "Set `emacsclient-status-need-die' and then kill the current buffer."
  (interactive)
  (let ((emacsclient-status-need-die t))
    (kill-buffer (current-buffer))))

(defun emacsclient-status-server-delete-client-advise (proc &optional noframe)
  "Advises before server deletes client to check if exit code should be generated."
  (when (or emacsclient-status-need-die (buffer-modified-p))
    (server-send-string proc "-error die\n")))

(defvar emacsclient-status-mode-map (make-sparse-keymap)
  "Keymap for `emacsclient-status-mode'.")

(define-key emacsclient-status-mode-map (kbd "C-c k") 'emacsclient-status-kill-current-buffer)
(define-key emacsclient-status-mode-map (kbd "C-c C-k") 'emacsclient-status-kill-current-buffer)

(define-minor-mode emacsclient-status-mode
  "Minor mode that causes emacsclient to return errors status.

When the buffer is killed forcefully or client is deleted while in a modified
state, a message is sent to emacsclient to die causing a non-zero status."
  :keymap emacsclient-status-mode-map
  :after-hook
  (if emacsclient-status-mode
      (advice-add 'server-delete-client :before #'emacsclient-status-server-delete-client-advise)
    (advice-remove 'server-delete-client #'emacsclient-status-server-delete-client-advise)))

(use-package server
  :init
  (server-start)
  :config
  (add-hook 'server-switch-hook 'emacsclient-status-mode))

;;;; begin packages
(use-package emacs
  :commands (swap-windows
	     rotate-windows
	     indent-buffer
	     buffer-enable-reindent
	     buffer-disable-reindent)
  :bind (("C-x C--" . rotate-windows)
	 ("C-x C-i" . imenu)
	 ("C-w" . kill-region-or-backward-word))
  :config
  (defun swap-windows (w1 w2)
    "Swaw window W1 and W2."
    (let ((b1 (window-buffer w1))
	  (b2 (window-buffer w2))
	  (s1 (window-start w1))
	  (s2 (window-start w2)))
      (set-window-buffer w1  b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))

  (defun rotate-windows ()
    "Rotate windows."
    (interactive)
    (let ((lst (window-list))
	  (sel (selected-window))
	  slst)
      (cond ((not (> (length lst) 1))
	     (message "You can't rotate a single window!"))
	    ((equal (car (last lst)) sel)
	     (swap-windows (car lst) (car (last lst))))
	    ((setq slst (member sel lst))
	     (swap-windows (car slst) (cadr slst)))))
    (other-window 1))

  (defun indent-buffer ()
    "Indent the current buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun buffer-enable-reindent ()
    "Enable `indent-buffer' on the current buffer."
    (interactive)
    (add-hook 'before-save-hook #'indent-buffer nil t))

  (defun buffer-disable-reindent ()
    "Disable `indent-buffer' on the current buffer."
    (interactive)
    (remove-hook 'before-save-hook #'indent-buffer t)))

(use-package whitespace
  :diminish
  :commands (buffer-disable-whitespace-cleanup
	     buffer-enable-whitespace-cleanup)
  :config
  (defun buffer-disable-whitespace-cleanup ()
    "Disable `whitespace-cleanup' in the current buffer."
    (interactive)
    (remove-hook 'before-save-hook #'whitespace-cleanup t))

  (defun buffer-enable-whitespace-cleanup ()
    "Enable `whitespace-cleanup' in the current buffer."
    (interactive)
    (add-hook 'before-save-hook #'whitespace-cleanup nil t)))

(use-package elisp-mode
  :hook ((emacs-lisp-mode . buffer-enable-reindent)
	 (emacs-lisp-mode . imenu-elisp-sections)
	 (emacs-lisp-mode . buffer-enable-whitespace-cleanup))
  :commands imenu-elisp-sections
  :config
  (defun imenu-elisp-sections ()
    "Find sections in elisp file denoted by `;;;'."
    (setq imenu-prev-index-position-function nil)
    (add-to-list 'imenu-generic-expression '(nil "^(use-package \\(.+\\)$" 1) t)
    (add-to-list 'imenu-generic-expression '(nil "^;;;; \\(.+\\)$" 1) t)))

(use-package magit
  :defer
  :ensure)

(use-package evil
  :ensure
  :bind (:map evil-normal-state-map
	      ("M-y" . counsel-yank-pop)
	      :map evil-motion-state-map
	      (";" . evil-ex)
	      (":" . evil-repeat-find-char)
	      :map evil-window-map
	      ("C-q" . evil-quit)
	      ("C-j" . evil-window-down)
	      ("C-k" . evil-window-up)
	      ("C-h" . evil-window-left)
	      ("C-l" . evil-window-right))
  :init
  (modify-syntax-entry ?_ "w")
  (evil-mode t)
  :config
  (evil-declare-motion 'flymake-goto-next-error)
  (evil-declare-motion 'flymake-goto-prev-error)

  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'diff-mode 'emacs))

(use-package undo-fu
  :ensure)

(use-package evil-leader
  :after evil
  :defer
  :ensure
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-key "gg" 'magit-status)
  (evil-leader/set-key "f" 'find-file))

(use-package selectrum
  :ensure
  :bind (:map selectrum-minibuffer-map
	      ("C-w" . selectrum-backward-kill-sexp)))

(use-package selectrum-prescient
  :ensure)

(provide 'init)
;;; init.el ends here
