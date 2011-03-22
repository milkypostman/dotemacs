;; Milkmacs
;;
;; Simple setup for Python and other things.
;; Autocompletion is setup automatically.
;; To complete using Rope completion hit M-/

;; basic configuration

;; debug if we would like
;; (setq debug-on-error t)

;; properly setup the environment
(push "/usr/local/bin" exec-path)
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))


;; Load all of my plugins
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; my function for adding all vendor specific directories (not
;; subdirectories) to the load-path and put them first!
(defun add-subdirs-load-path (default-directory)
  (let* ((dirs (directory-files default-directory)))
    (dolist (dir dirs)
      (unless (member dir '("." ".." "RCS" "CVS" "rcs" "cvs"))
	(let ((fullpath (concat default-directory dir)))
	  (when (file-directory-p dir)
	    (add-to-list 'load-path fullpath)))))))

(add-subdirs-load-path "~/.emacs.d/vendor/")
(add-subdirs-load-path "~/.emacs.d/themes/")


;; do we want VIM mode?
;; (require 'vimpulse)

;; start the server
(server-start)

;; backup settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   )

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

(setq default-indicate-buffer-boundaries (quote left))
;; (add-to-list 'default-fringe-indicator-alist '(empty-line . empty-line))

(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq line-number-mode t)
(setq column-number-mode t)
(setq set-mark-command-repeat-pop t)

(set-default 'indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'auto-mode-alist '("\\.bashrc_.*" . sh-mode))

;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

(require 'milkmacs-bindings)
(require 'milkmacs-defun)
(require 'milkmacs-fringemark)
(require 'milkmacs-ido)
(require 'milkmacs-markdown)
(require 'milkmacs-python)
(require 'milkmacs-ruby)
(require 'milkmacs-visual)

;; magit
(autoload 'magit-status "magit" "Function for managing git" t)
(global-set-key "\C-xg" 'magit-status)

;; save place
(require 'saveplace)
(setq-default save-place t)

(require 'misc)
(autoload 'beginning-or-indentation "misc-cmds")

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

(require 'auto-complete-config)
(ac-config-default)
(setq-default ac-sources (append (list 'ac-source-yasnippet) ac-sources))


;; erlang
(require 'erlang-start)

;; scala
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
	  (lambda ()
	    (yas/load-directory "~/.emacs.d/vendor/scala-mode/contrib/yasnippet/snippets")
	    ))

;; custom stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; yasnippet -- really slow so don't load it less we're on the desktop
(autoload 'yas/initialize "yasnippet" "Initialize yasnippet")
(defun yas-initialize () (interactive) (yas/initialize))
(defadvice yas/initialize (before yas-load-directories activate)
  "Load my snippet directories when initializing."
  (progn
    (yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
    (yas/load-directory "~/.emacs.d/snippets")))

(if (window-system)
    (yas/initialize))

;; load system specific stuff.
(ignore-errors
  (load (concat "~/.emacs.d/milk/" (symbol-name system-type) ".el")))

