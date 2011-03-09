;; Milkmacs
;; 
;; Simple setup for Python and other things.
;; Autocompletion is setup automatically.
;; To complete using Rope completion hit M-/

;; basic configuration

;; properly setup the environment
(push "/usr/local/bin" exec-path)
(setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 (getenv "PATH")
	 )
	)

;; Load all of my plugins
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))
(cd "~/.emacs.d/")

;; external modules
;; do we want VIM mode
;; (require 'vimpulse)

;; start the server
(server-start)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(setq auto-save-file-name-transforms `((".*", "~/.emacs.d/autosave")))

(setq default-indicate-buffer-boundaries (quote left))
;; (add-to-list 'default-fringe-indicator-alist '(empty-line . empty-line))

(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'hyper)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-bar-mode nil)

(set-default 'indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)

(add-to-list 'auto-mode-alist '("\\.bashrc_.*" . sh-mode))


;; keybindings

;; autoindent
(global-set-key (kbd "RET") 'newline-and-indent)

;; make ctrl-w work as expected
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-z" 'other-window)
(global-set-key "\M-`" 'other-frame)

;; (global-set-key [?\C-6] (lambda ()
;; 			  (interactive)
;; 			  (switch-to-buffer (other-buffer))))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; hack alternative for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-unset-key "\C-xm") ; disable mail

;; remap the delete key, who needs help?
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [(hyper h)] 'help-command)

;; misc commands stolen from the starter kit
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; prefixing window commands is a pain
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-4") 'ctl-x-4-prefix)
(global-set-key (kbd "C-5") 'ctl-x-5-prefix)



;; change M-w to copy the line if no region selected
;; WARN: replaces function
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; change C-x C-k to kill the line if no region selected
;; WARN: replaces function
(put 'kill-region 'interactive-form      
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))


;; color theming
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))
;; global hl mode doesn't look good with hober!
;; (global-hl-line-mode 1)


;; misc useful functions
;; http://www.emacswiki.org/cgi-bin/wiki/misc-cmds.el
(require 'misc-cmds)
(global-set-key "\C-a" 'beginning-or-indentation)

;; recent files
(recentf-mode 1)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(define-key ctl-x-4-map "f" 'recentf-ido-find-file-other-window)
(global-set-key "\C-c\C-t" 'idomenu)
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun recentf-ido-find-file-other-window ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file-other-window file))))


;; magit
(autoload 'magit-status "magit.el" "Function for managing git" t)
;; (require 'magit)
(global-set-key "\C-cms" 'magit-status)

;; cua mode
(setq cua-enable-cua-keys nil)
(cua-mode t)


;; save place
(require 'saveplace)
(setq-default save-place t)


;; markdown
;; use autoload because it delays loading the function until we need it.
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; yasnippet
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/snippets")


;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

(require 'auto-complete-config)
(ac-config-default)
(setq-default ac-sources (append (list 'ac-source-yasnippet) ac-sources))



;; ido-mode
(require 'ido)
;; http://www.emacswiki.org/emacs/idomenu.el
(require 'idomenu)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

(add-hook 'ido-setup-hook
	  (lambda ()
	    (setq ido-mode-map ido-completion-map)
	    (define-key ido-mode-map "\C-h" 'ido-delete-backward-updir)
	    (define-key ido-mode-map "\C-w" 'ido-delete-backward-word-updir)
	    (define-key ido-mode-map "\C-n" 'ido-next-match)
	    (define-key ido-mode-map "\C-p" 'ido-prev-match)
	    (define-key ido-completion-map [tab] 'ido-complete)
	    ))

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s") 
  (lambda() 
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))


;; setup Python path properly
;; (require 'python-mode) ; use specialized python-mode
(if (string-equal (shell-command-to-string "uname -s") "Darwin\n")
    (setenv "PYTHONPATH" 
	    "/Users/dcurtis/Development/compepi:/Users/dcurtis/Development/networkx"))

;; pymacs
(require 'pymacs)

;; function to load pymacs when python file is loaded
(defvar ropemacs-loaded nil)
(defun ropemacs-require ()
  (with-no-warnings
    (unless ropemacs-loaded
      (pymacs-load "ropemacs" "rope-")
      (if (boundp 'ropemacs-enable-autoimport)
	  (setq ropemacs-guess-project t))
      (if (boundp 'ropemacs-enable-autoimport)
	  (setq ropemacs-enable-autoimport t))
      (setq ropemacs-loaded t)
      (ropemacs-mode)
      ))
  (cond ((file-exists-p ".ropeproject")
	 (rope-open-project default-directory))
	((file-exists-p "../.ropeproject")
	 (rope-open-project (concat default-directory "..")))
	)
  )

(defun python-mode-setup ()
  (define-key python-mode-map "\C-c\C-e" 'python-end-of-block)
  (define-key python-mode-map "\C-\M-d" 'python-end-of-block)
  (define-key python-mode-map "\C-c\C-e" 'python-end-of-block)
  (define-key python-mode-map "\C-\M-d" 'python-end-of-block)
  )

;; (defun specialtab ()
;;   (interactive)
;;   (unless (let ((yas/fallback-behavior 'return-nil)) (yas/expand))
;;     (ac-start)
;;     )
;;   )

;; (global-set-key "\t" 'specialtab)
  
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(add-hook 'python-mode-hook 'ropemacs-require)
(add-hook 'python-mode-hook 'python-mode-setup)

;; (global-set-key [(hyper /)] 'hippie-expand)
;; (setq hippie-expand-try-functions-list '(yas/hippie-try-expand try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; custom stuff
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(scroll-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Menlo")))))
