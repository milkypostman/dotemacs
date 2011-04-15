;; Milkmacs
;;
;; Simple setup for Python and other things.
;; Autocompletion is setup automatically.
;; To complete using Rope completion hit M-/

;; basic configuration
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; debug if we would like
(setq debug-on-error t)

;; properly setup the environment
(push "/usr/local/bin" exec-path)

(setenv "PATH"
	(mapconcat 'identity
		   (delete-dups
		    (append (list "/usr/local/bin" "~/bin" "/usr/texbin")
			    (split-string (getenv "PATH") ":")))
		   ":"))



;; Load all of my plugins
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("kieranhealy" . "http://kieranhealy.org/packages/") t)
(add-to-list 'package-archives '("josh" . "http://josh.github.com/elpa/") t)

;; required for cssh
(package-initialize)



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
(add-to-list 'load-path "~/.emacs.d/vendor/ess/lisp/")


;; do we want VIM mode?
;; (require 'vimpulse)

;; start the server
(server-start)



;; don't be poppin' new frames
(setq ns-pop-up-frames nil)



;; backup settings
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)


(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
(setq delete-auto-save-files nil)

(setq default-indicate-buffer-boundaries (quote left))

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
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

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
;; (global-set-key (kbd "C-0") 'delete-window)
;; (global-set-key (kbd "C-1") 'delete-other-windows)
;; (global-set-key (kbd "C-2") 'split-window-vertically)
;; (global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-4") 'ctl-x-4-prefix)
(global-set-key (kbd "C-5") 'ctl-x-5-prefix)
(global-set-key (kbd "C-.") 'repeat)

;; To help Unlearn C-x 0, 1, 2, o
;; (global-unset-key (kbd "C-x 5")) ; was ctl-x-5-prefix
;; (global-unset-key (kbd "C-x 4")) ; was ctl-x-4-prefix
;; (global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
;; (global-unset-key (kbd "C-x 2")) ; was split-window-vertically
;; (global-unset-key (kbd "C-x 1")) ; was delete-other-windows
;; (global-unset-key (kbd "C-x 0")) ; was delete-window
;; (global-unset-key (kbd "C-x o")) ; was other-window

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-M-;") 'comment-dwim-line)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)

(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-C-r") 'replace-regexp)

(global-set-key (kbd "M-C-m")   'open-next-line)
;; (global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

;; misc useful functions
;; http://www.emacswiki.org/cgi-bin/wiki/misc-cmds
(autoload 'beginning-or-indentation "misc-cmds")
(global-set-key "\C-a" 'beginning-or-indentation)

(eval-after-load "dired"
  '(define-key dired-mode-map "F" 'dired-find-file-other-frame))

;; defun

(defun dired-find-file-other-frame ()
  "In Dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))


;; emacsclient map
(defun server-edit-save ()
  "Save the file and exit server mode."
  (interactive)
  (save-buffer)
  (server-edit))

;; (add-hook 'server-visit-hook '(lambda ()
;; 				(setq save-place nil)
;; 				(local-set-key (kbd "C-c C-c") 'server-edit-save)))

;; automatically recursive search
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    ))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-'") 'push-mark-no-activate)

(defun exchange-point-and-mark-no-activate (&optional arg)
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive "P")
  (let ((region-active (region-active-p)))
    (exchange-point-and-mark)
    (if (or arg region-active) (activate-mark)
      (deactivate-mark))))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defadvice jump-to-register (before jump-to-register-advice activate)
  (push-mark (point) t nil))




(defun special-open-line (n)
  (interactive "p")
  (save-excursion
    (while (> n 0)
      (newline)
      (setq n (1- n)))
    (if (not (eolp)) (indent-according-to-mode))
    )
  (if (bolp) (indent-according-to-mode))
  )
(global-set-key (kbd "C-o") 'special-open-line)


;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (newline-and-indent))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (if (eolp) (save-excursion (delete-region (point) (progn (skip-chars-backward " \t") (point)))))
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(defun make-executable ()
  "Make the current file loaded in the buffer executable"
  (interactive)
  (if (buffer-file-name)
      (shell-command
       (mapconcat 'identity
		  (list "chmod" "u+x" (shell-quote-argument (buffer-file-name))) " "))
    (message "Buffer has no filename.")))

;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))


;; save place
(require 'saveplace)
(setq-default save-place t)

(require 'misc)

;; (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

(setq hippie-expand-try-functions-list (delq 'try-expand-line hippie-expand-try-functions-list))
(add-to-list 'hippie-expand-try-functions-list 'try-expand-line t)

(defun fancy-tab (arg)
  (interactive "P")
  (setq this-command last-command)
  (if (or (eq this-command 'hippie-expand) (looking-at "\\_>"))
      (progn
	(setq this-command 'hippie-expand)
	(hippie-expand arg))
    (setq this-command 'indent-for-tab-command)
    (indent-for-tab-command arg)))


(define-key read-expression-map [(tab)] 'hippie-expand)
(global-set-key (kbd "TAB") 'fancy-tab)


(add-hook 'emacs-lisp-mode-hook 'move-lisp-completion-to-front)
(defun move-lisp-completion-to-front ()
  "Adjust hippie-expand-try-functions-list to have lisp completion at the front."
  (make-local-variable 'hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list
	(append (list 'yas/hippie-try-expand 'try-complete-lisp-symbol-partially 'try-complete-lisp-symbol)
		(delq 'yas/hippie-try-expand
		      (delq 'try-complete-lisp-symbol-partially
			    (delq 'try-complete-lisp-symbol
				  hippie-expand-try-functions-list))))))

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


;; ido recent files
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(define-key ctl-x-4-map "f" 'recentf-ido-find-file-other-window)
(global-set-key "\C-c\C-t" 'idomenu)
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (recentf-mode 1)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun recentf-ido-find-file-other-window ()
  "Find a recent file using ido."
  (interactive)
  (recentf-mode 1)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file-other-window file))))

(defun ido-yank ()
  (interactive)
  (insert-for-yank
   (ido-completing-read "Select kill: " kill-ring)))

;; yasnippet -- really slow so don't load it less we're on the desktop
;; (autoload 'yas/initialize "yasnippet" "Initialize yasnippet")
;; (defun yas-initialize () (interactive) (yas/initialize))
(eval-after-load 'yasnippet
  '(progn
    (yas/load-directory "~/.emacs.d/snippets")))
;; (yas/load-directory "~/.emacs.d/snippets")

(global-set-key (kbd "C-x g") 'magit-status)

;; auctex
(eval-after-load 'latex
  '(progn
     (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
     (setq TeX-source-correlate-method 'synctex)


     (setq TeX-auto-save t)
     (setq TeX-parse-self t)
     (setq TeX-save-query nil)
     (setq-default TeX-PDF-mode t)
     ;; (setq-default TeX-master nil)
     ;; (setq LaTeX-command "latex")
     (setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
     (setq TeX-view-program-selection '((output-pdf "Skim")))
     (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     (setq reftex-plug-into-AUCTeX t)
     (define-key TeX-mode-map (kbd "C-c C-c")
       (lambda ()
	 (interactive)
	 (TeX-save-document (TeX-master-file))
	 (TeX-command "LaTeX" 'TeX-master-file)
	 ))
     ))

(eval-after-load 'reftex
  '(progn
     (add-to-list 'reftex-section-prefixes '(1 . "chap:"))))


;; erlang
(require 'erlang-start)


;; scala
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
	  (lambda ()
	    (yas/load-directory "~/.emacs.d/vendor/scala-mode/contrib/yasnippet/snippets")
	    ))


;; R
;; manually do autoloads so the whole shebang doesn't load everytime.
;; I hardly use R.
;; (require 'ess-site)
(autoload 'R-mode "ess-site" "R mode")
(autoload 'R "ess-site" "R inferior shell" t)
(setq auto-mode-alist
   (cons '("\\.[rR]\\'"	. R-mode) auto-mode-alist))


;; markdown
;; use autoload because it delays loading the function until we need it.
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook '(lambda ()
				 (auto-fill-mode 1)
				 (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
				 ))


;; python

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(defun setup-virtualenv ()
  "Setup virtualenv"
  (ignore-errors
    ;; setup virtualenv for the python-shell-process-environment
    (push "~/.virtualenvs/default/bin" exec-path)
    (setenv "PATH"
	    (concat
	     "~/.virtualenvs/default/bin" ":"
	     (getenv "PATH")
	     ))
    (add-hook 'python-mode-hook
	      (lambda ()
		(setq python-shell-process-environment
		      (list
		       (format "PATH=%s" (mapconcat
					  'identity
					  (reverse
					   (cons (getenv "PATH")
						 '("~/.virtualenvs/default/bin/")))
					  ":"))
		       "VIRTUAL_ENV=~/.virtualenvs/default/"))
		(setq python-shell-exec-path '("~/.virtualenvs/default/bin/"))))
    ))

(defun setup-ropemacs ()
  "Setup ropemacs"
  (with-temp-buffer
    (cd "~")
    (pymacs-load "ropemacs" "rope-"))
  ;; (ignore-errors (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)
  )

;; (require 'auto-complete)
;; (defvar ac-ropemacs-last-candidates)
;; (defun ac-ropemacs-candidates ()
;;   (setq ac-ropemacs-last-candidates ())
;;   (mapcar (lambda (item)
;;             (let ((name (car item))
;;                   (doc (cadr item))
;;                   (type (caddr item)))
;;               (add-to-list 'ac-ropemacs-last-candidates
;;                            (cons (concat ac-prefix name) doc))
;;               (concat ac-prefix name)))
;;           (rope-extended-completions)))

;; (defun ac-ropemacs-document (name)
;;   (let ((item (assoc name ac-ropemacs-last-candidates)))
;;     (if item (cdr item))))

;; (ac-define-source nropemacs
;;   '((candidates . ac-ropemacs-candidates)
;;     (symbol     . "p")
;;     (document   . ac-ropemacs-document)
;;     (cache      . t)))

;; (ac-define-source nropemacs-dot
;;   '((candidates . ac-ropemacs-candidates)
;;     (symbol     . "p")
;;     (document   . ac-ropemacs-document)
;;     (cache      . t)
;;     (prefix     . c-dot)
;;     (requires   . 0)))

;; (defun ac-python-mode-setup ()
;;   (setq ac-sources
;;         '(ac-source-nropemacs ac-source-nropemacs-dot ac-source-yasnippet)))

;; (defun ac-self-insert-and-complete ()
;;   (interactive)
;;   (self-insert-command 1)
;;   (ac-start))

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)


(eval-after-load 'pymacs
  (setq pymacs-python-command "/Users/dcurtis/.virtualenvs/default/bin/python")
  )

(eval-after-load 'python
  '(progn
     (setup-ropemacs)
     (setup-virtualenv)
     (define-key python-mode-map (kbd "C-h") 'python-indent-dedent-line-backspace)
     ))

(add-hook 'before-save-hook 'delete-trailing-whitespace nil nil)

(eval-after-load 'flymake
  '(progn
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
			  'flymake-create-temp-inplace))
	      (local-file (file-relative-name
			   temp-file
			   (file-name-directory buffer-file-name))))
	 (list "epylint" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
		  '("\\.py\\'" flymake-pylint-init))))

(require 'flymake-cursor)

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))





;; (defadvice flymake-mode (after post-command-stuff activate compile)
;;   "add keybindings"
;;    (local-set-key "\M-p" 'flymake-goto-prev-error)
;;    (local-set-key "\M-n" 'flymake-goto-next-error))


;; ;; ruby
;; (autoload 'run-ruby "inf-ruby"
;;   "Run an inferior Ruby process")
;; (autoload 'inf-ruby-keys "inf-ruby"
;;   "Set local key defs for inf-ruby in ruby-mode")
;; (add-hook 'ruby-mode-hook
;; 	  '(lambda ()
;; 	     (inf-ruby-keys)
;; 	     ))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c C-c") 'ruby-run-w/compilation)
     ))



;; visual

;; color theming
;; (autoload 'color-theme-initialize "color-theme")
(require 'color-theme)
;; backup current color theme
(fset 'color-theme-snapshot (color-theme-make-snapshot))

(defun color-theme-undo ()
  (interactive)
  ;; (color-theme-reset-faces)
  (color-theme-snapshot))

;; (color-theme-initialize)
;; (require 'zenburn)
;; (color-theme-zenburn)
;; (require 'color-theme-hober2)
;; (color-theme-hober2)
;; (require 'color-theme-twilight)
(load "~/.emacs.d/themes/twilight/color-theme-twilight.el")
(color-theme-twilight)
;; (require 'color-theme-inkpot)
;; (color-theme-inkpot)
;; (set-face-attribute 'hl-line nil
;; 		    :inherit 'unspecified
;; 		    :background "gray8")
;; (set-face-foreground 'hl-line nil)
;; (set-face-background 'hl-line nil)
;; (require 'color-theme-ir-black)
;; (color-theme-ir-black)
;; (load "~/.emacs.d/themes/vibrant-ink/color-theme-vibrant-ink.el")
;; (color-theme-vibrant-ink)
;; (set-face-background 'default "black")
;; (require 'color-theme-complexity)
;; (color-theme-complexity)
;; (load "~/.emacs.d/themes/merbivore/color-theme-merbivore.el")
;; (color-theme-merbivore)


;; global hl mode doesn't look good with hober!
(if (not (window-system))
    (menu-bar-mode 0)
  (require 'fringemark)
  (set-face-font 'default "Menlo")
  )


(global-hl-line-mode 1)
(show-paren-mode 1)

;; custom stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))


;; System Specific Settings
(cond ((eq system-type 'darwin)
       (setenv "PYTHONPATH" "/Users/dcurtis/Development/compepi:/Users/dcurtis/Development/networkx"))
      )


