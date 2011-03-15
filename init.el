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
(setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 (getenv "PATH")
	 )
	)


;; Load all of my plugins
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")
;; my function for adding all vendor specific directories (not subdirectories) to the load-path
;; and put them first!
(let* ((default-directory "~/.emacs.d/vendor/") (dirs (directory-files default-directory)))
  (dolist (dir dirs)
    (unless (member dir '("." ".." "RCS" "CVS" "rcs" "cvs"))
      (let ((fullpath (concat default-directory dir)))
	(when (file-directory-p dir)
	  (add-to-list 'load-path fullpath))))))


;; do we want VIM mode?
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
(setq mac-option-modifier 'super)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-bar-mode nil)

(set-default 'indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)
(transient-mark-mode 0)

(add-to-list 'auto-mode-alist '("\\.bashrc_.*" . sh-mode))


;; keybindings

;; make C-tab bet M-tab
(define-key function-key-map [(control tab)] [?\M-\t])

;; autoindent
(global-set-key (kbd "RET") 'newline-and-indent)

;; make ctrl-w work as expected
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-z") 'other-window)
(global-set-key (kbd "M-`") 'other-frame)

;; (global-set-key [?\C-6] (lambda ()
;; 			  (interactive)
;; 			  (switch-to-buffer (other-buffer))))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; hack alternative for M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-unset-key (kbd "C-x m")) ; disable mail

;; remap the delete key, who needs help?
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-c h") 'help-command)

;; misc commands stolen from the starter kit
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; prefixing window commands is a pain
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-4") 'ctl-x-4-prefix)
(global-set-key (kbd "C-5") 'ctl-x-5-prefix)
(global-set-key (kbd "C-.") 'repeat)


;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 5")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 4")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(global-set-key (kbd "M-C-m")   'open-next-line)
;; (global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))


;; misc useful functions
;; http://www.emacswiki.org/cgi-bin/wiki/misc-cmds
;; (require 'misc-cmds)
(autoload 'beginning-or-indentation "misc-cmds")
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
	       	  

;; textmate.el
;; (require 'textmate)

;; (require 'test-case-mode)
(autoload 'test-case-mode "test-case-mode" nil t)
(autoload 'enable-test-case-mode-if-test "test-case-mode")
(autoload 'test-case-find-all-tests "test-case-mode" nil t)
(autoload 'test-case-compilation-finish-run-all "test-case-mode")

	       	  
;; magit      
(autoload 'magit-status "magit" "Function for managing git" t)
(global-set-key "\C-xg" 'magit-status)

;; cua mode
;; (setq cua-enable-cua-keys nil)
;; (setq cua-highlight-region-shift-only t)
;; (setq cua-toggle-set-mark nil)
;; (cua-mode t)


;; save place
(require 'saveplace)
(setq-default save-place t)

;; markdown
;; use autoload because it delays loading the function until we need it.
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; yasnippet
;; (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
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
;; (autoload 'ido-mode "ido")
(require 'ido)

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

;; http://www.emacswiki.org/emacs/idomenu
(autoload 'idomenu "idomenu")


;; ;; python-mode.el
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;; 				   interpreter-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)

;; setup Python path properly
(if (string-equal (shell-command-to-string "uname -s") "Darwin\n")
    (setenv "PYTHONPATH" "/Users/dcurtis/Development/compepi:/Users/dcurtis/Development/networkx"))

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)


(defun setup-ropemacs ()
  "Setup ropemacs"
  (pymacs-load "ropemacs" "rope-")

  ;; (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)

  (add-hook 'python-mode-hook
	    (lambda ()
	      (cond ((file-exists-p ".ropeproject")
		     (rope-open-project default-directory))
		    ((file-exists-p "../.ropeproject")
		     (rope-open-project (concat default-directory "..")))
		    )))
  )

;; python.el by fabia'n
;; (add-to-list 'load-path "~/.emacs.d/vendor/python.el")
;; (require 'python)

;;(autoload 'python-mode "python" "Python editing mode." t)
(eval-after-load 'python-mode
  '(progn
     (setup-ropemacs)
     ))

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; custom stuff
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(scroll-bar-mode nil))


;; color theming
;; (autoload 'color-theme-initialize "color-theme")
(require 'color-theme)
;; (require 'zenburn)
;; (color-theme-zenburn)
(require 'color-theme-hober2)
(color-theme-hober2)
;; global hl mode doesn't look good with hober!
;; (global-hl-line-mode 1)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Menlo")))))
