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
(setq set-mark-command-repeat-pop t)

(set-default 'indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-'") 'push-mark-no-activate)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

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
;; (global-set-key (kbd "C-4") 'ctl-x-4-prefix)
;; (global-set-key (kbd "C-5") 'ctl-x-5-prefix)
;; (global-set-key (kbd "C-.") 'repeat)


;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 5")) ; was ctl-x-5-prefix
(global-unset-key (kbd "C-x 4")) ; was ctl-x-4-prefix
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)

(require 'misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)

(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-C-r") 'replace-regexp)

(defadvice jump-to-register (before jump-to-register-advice activate)
  (push-mark (point) t nil))

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

;; (put 'kill-ring-save 'interactive-form
;;      '(interactive
;;        (if (use-region-p)
;;            (list (region-beginning) (region-end))
;;          (list (line-beginning-position) (line-beginning-position 2)))))

;; (put 'kill-region 'interactive-form
;;      '(interactive
;;        (if (use-region-p)
;;            (list (region-beginning) (region-end))
;;          (list (line-beginning-position) (line-beginning-position 2)))))

;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))


;; misc useful functions
;; http://www.emacswiki.org/cgi-bin/wiki/misc-cmds
(autoload 'beginning-or-indentation "misc-cmds")
(global-set-key "\C-a" 'beginning-or-indentation)

(if window-system
    (progn
      ;; overlay an arrow where the mark is
      (defvar mp-overlay-arrow-position)
      (make-variable-buffer-local 'mp-overlay-arrow-position)
      ;; (delq 'mp-overlay-arrow-position overlay-arrow-variable-list)
      (add-to-list 'overlay-arrow-variable-list  'mp-overlay-arrow-position)
      (defun mp-mark-hook ()
	;; (make-local-variable 'mp-overlay-arrow-position)
	(unless (or (minibufferp (current-buffer)) (not (mark)))
	  (set
	   'mp-overlay-arrow-position
	   (save-excursion
	     (goto-char (mark))
	     (forward-line 0)
	     (point-marker)))))
      (add-hook 'post-command-hook 'mp-mark-hook)

      ;; make the mark fringe bitmap look cool dude
      (define-fringe-bitmap 'mp-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
      (put 'mp-overlay-arrow-position 'overlay-arrow-bitmap 'mp-hollow-right-arrow)
      ))


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

(add-hook 'markdown-mode-hook '(lambda ()
				 (auto-fill-mode 1)))


;; emacsclient map
(defun server-edit-save ()
  "Save the file and exit server mode."
  (interactive)
  (save-buffer)
  (server-edit))
(add-hook 'server-visit-hook '(lambda ()
				(setq save-place nil)
				(local-set-key (kbd "C-c C-c") 'server-edit-save)))



;; yasnippet -- really slow so don't load it less we're on the desktop

(defun mp-load-yasnippet ()
  ;; (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
  (yas/load-directory "~/.emacs.d/snippets")
  )


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


;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)


(defun setup-ropemacs ()
  "Setup ropemacs"
  (ignore-errors (pymacs-load "ropemacs" "rope-")


		 ;; (setq ropemacs-codeassist-maxfixes 3)
		 (setq ropemacs-guess-project t)
		 (setq ropemacs-enable-autoimport t)

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
			     (setq python-shell-exec-path '("~/.virtualenvs/default/bin/"))
			     (cond ((file-exists-p ".ropeproject")
				    (rope-open-project default-directory))
				   ((file-exists-p "../.ropeproject")
				    (rope-open-project (concat default-directory "..")))
				   ))))
  )

;; python.el by fabia'n
;; (add-to-list 'load-path "~/.emacs.d/vendor/python.el")
;; (require 'python)

;;(autoload 'python-mode "python" "Python editing mode." t)
(eval-after-load 'python
  '(progn
     (setup-ropemacs)
     (require 'virtualenv)
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


;; RUBY
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)
	     ))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c C-c") 'ruby-run-w/compilation)
     ))


;; custom stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; color theming
;; (autoload 'color-theme-initialize "color-theme")
(require 'color-theme)
;; (color-theme-initialize)
;; (require 'zenburn)
;; (color-theme-zenburn)
;; (require 'color-theme-hober2)
;; (color-theme-hober2)
;; (set-face-attribute 'hl-line nil
;; 		    :inherit 'unspecified
;; 		    :background "gray8")
;; (set-face-foreground 'hl-line nil)
;; (set-face-background 'hl-line nil)
;; (require 'color-theme-irblack)
;; (color-theme-irblack)

;; global hl mode doesn't look good with hober!
(global-hl-line-mode 1)


;; setup Python path properly
(if (string-equal (shell-command-to-string "uname -s") "Darwin\n")
    (progn
      (setenv "PYTHONPATH" "/Users/dcurtis/Development/compepi:/Users/dcurtis/Development/networkx")

      (mp-load-yasnippet)

      (set-face-font 'default "Menlo")))
