;;; init.el -- Milkmacs configuration file
;;
;; based on emacs-starter-kit
;;

;;; initialization
(require 'cl)

;; all functions defined in `defun'
(add-to-list 'load-path "~/.emacs.d/")

;;; packages
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(require 'checkdoc)
(require 'midnight)
(require 'misc)
(require 'recentf)
(require 'saveplace)
(require 'uniquify)

;;; functions
(require 'defun)

(setq mp-extra-paths
      '("~/.virtualenv/bin/"
	"~/.cabal/bin/"
	"~/bin/"
	"/usr/local/bin/"
	"/usr/texbin/"))

(setenv "PATH"
	(mapconcat
	 'identity
	 (delete-dups
	  (append
	   (mapcar (lambda (path)
		     (if (string-match "^~" path)
			 (replace-match (getenv "HOME") nil nil path)
		       path)) mp-extra-paths)
	   (split-string (getenv "PATH") ":")))
	 ":"))

(mapc (lambda (path) (push path exec-path)) mp-extra-paths)

(ignore-errors (server-start))

;;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'eshell/ff 'find-file)
(defalias 'eshell/ffow 'find-file-other-window)
(defalias 'yes-or-no-p 'y-or-n-p)


;;; random number generator
(random t)

;;; remaps
(define-key key-translation-map (kbd "<C-tab>") (kbd "M-TAB"))
(define-key key-translation-map (kbd "C-x C-m") (kbd "M-x"))
(define-key key-translation-map (kbd "C-x C-d") (kbd "C-x d"))


;;; global key bindings
(global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-M-SPC") 'just-one-space)
(global-set-key (kbd "A-h") 'ns-do-hide-emacs)
(global-set-key (kbd "A-M-h") 'ns-do-hide-others)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "M-i") 'back-to-indentation)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-M-h") 'mark-defun)
(global-set-key (kbd "C-M-,") 'beginning-of-buffer-other-window)
(global-set-key (kbd "M-`") 'other-frame)

(global-set-key (kbd "C-4") 'ctl-x-4-prefix)

(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)

(global-set-key (kbd "M-C-m") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c r") 'iterm-run-previous-command)


(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-h") (kbd "<DEL>"))

(global-set-key (kbd "C-c h") 'help-command)

(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)
22
(global-set-key (kbd "C-c w") 'copy-paste)

(global-set-key (kbd "C-x C-i") 'imenu)

(define-key ctl-x-4-map (kbd "f") 'ido-find-file-other-window)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)


;;; macros
(defmacro after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))


;;; advice
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;;; hooks
(add-hook 'write-file-functions 'time-stamp)


;;; generic
(blink-cursor-mode nil)
(global-auto-revert-mode t)
(fringe-mode 0)
(mac-mouse-wheel-mode t)
(menu-bar-mode nil)
(mouse-wheel-mode t)
(recentf-mode t)
(savehist-mode t)
(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)
(visual-line-mode -1)
(winner-mode t)

;;; the uncustomizable
(setq ring-bell-function 'ignore)
(setq redisplay-dont-pause t)
(setq whitespace-style '(face trailing lines-tail space-before-tab indentation space-after-tab))

(setq ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
(setq ansi-color-for-comint-mode t)
(setq ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
(setq auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosave/" t))))
(setq backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))
(setq cua-enable-cua-keys nil)
(setq custom-theme-directory "~/.emacs.d/themes/")
(setq default-frame-alist (quote ((font . "Monaco-10"))))
(setq delete-auto-save-files nil)
(setq diff-switches "-u")
(setq enable-recursive-minibuffers t)
(setq flymake-gui-warnings-enabled t)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq imenu-auto-rescan t)
(setq indent-tabs-mode nil)
(setq indicate-empty-lines t)
(setq inhibit-startup-echo-area-message "dcurtis")
(setq inhibit-startup-screen t)
(setq ispell-extra-args (quote ("--sug-mode=ultra")))
(setq ispell-program-name "aspell")
(setq line-spacing 2)
(setq mode-line-in-non-selected-windows t)
(setq mode-line-inverse-video t)
(setq mouse-wheel-scroll-amount (quote (0.01)))
(setq mouse-yank-at-point t)
(setq ns-alternate-modifier (quote super))
(setq ns-command-modifier (quote meta))
(setq ns-pop-up-frames nil)
(setq ns-right-alternate-modifier (quote alt))
(setq ns-tool-bar-display-mode 'both)
(setq ns-tool-bar-size-mode 'regular)
(setq save-place t)
(setq save-place-file "~/.emacs.d/places")
(setq scroll-conservatively 5)
(setq scroll-margin 5)
(setq send-mail-function (quote mailclient-send-it))
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq shift-select-mode nil)
(setq split-height-threshold nil)
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")
(setq tramp-remote-path '(tramp-default-remote-path
			  tramp-own-remote-path
			  "/bin"
			  "/usr/bin"
			  "/usr/sbin"
			  "/usr/local/bin"
			  "/local/bin"
			  "/local/freeware/bin"
			  "/local/gnu/bin"
			  "/usr/freeware/bin"
			  "/usr/pkg/bin"
			  "/usr/contrib/bin"))
(setq truncate-lines t)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-separator " • ")
(setq user-full-name "Donald Ephraim Curtis")
(setq user-mail-address "dcurtis@milkbox.net")
(setq visible-bell nil)

;;;; darwin Specific
(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")
       (setenv
	"PYTHONPATH"
	"/Users/dcurtis/src/compepi:/Users/dcurtis/src/networkx")))

;;;; graphical settings
(when window-system
  (menu-bar-mode t)
  ;; specify a unicode font : MENLO (forced normal)
  (set-fontset-font "fontset-default" 'unicode "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))



;;; dired
(global-set-key (kbd "C-x C-j") 'dired-jump)
(define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)

(after 'dired
       (define-key dired-mode-map (kbd "M-p") 'dired-back-to-top)
       (define-key dired-mode-map (kbd "M-n") 'dired-jump-to-bottom)
       (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files))

;;; ibuffer
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("115" (filename . "115"))
	 ("325" (filename . "325"))
	 ("705" (filename . "705"))
	 ("345" (filename . "345"))
	 ("455" (filename . "455"))
	 ("dirs" (or
		  (mode . dired-mode)
		  (mode . wdired-mode)))
	 ("notes" (filename . "Elements"))
	 ("magit" (name . "\*magit"))
	 ("help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*")))
	 ("econfig" (or (filename . ".emacs.d")
			(filename . "init.el"))))))


(defun mp-ibuffer-hook ()
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'mp-ibuffer-hook)


;;; ido

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-max-prospects 10)
(setq ido-read-file-name-non-ido nil)
(setq ido-use-filename-at-point (quote guess))
(setq ido-use-virtual-buffers t)

;; (defun mp-ido-edit-input ()
;;   "Edit absolute file name entered so far with ido; terminate by RET.
;; oIf cursor is not at the end of the user input, move to end of input."
;;   (interactive)
;;   (if (not (eobp))
;;       (end-of-line)
;;     (setq ido-text-init ido-text)
;;     (setq ido-exit 'edit)
;;     (exit-minibuffer)))

(defun mp-ido-hook ()
  (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  ;; (define-key ido-completion-map (kbd "C-e") 'mp-ido-edit-input)
  (define-key ido-completion-map [tab] 'ido-complete))

(add-hook 'ido-setup-hook 'mp-ido-hook)


;;; ido-ubiquitous
(after 'ido-ubiquitous
       (ido-ubiquitous-disable-in evil-ex))

;; (ido-ubiquitous-mode t)
;; (setq ido-ubiquitous-command-exceptions (quote (evil-ex execute-extended-command)))
;; (setq ido-ubiquitous-function-exceptions (quote (grep-read-files ucs-insert)))

;;; smex
(after 'smex-autoloads (smex-initialize))

(after 'smex
       (global-set-key (kbd "M-x") 'smex)
       (global-set-key (kbd "M-X") 'smex-major-mode-commands)
       (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;;; ispell
(setq ispell-list-command "list")


;;; diff commands
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))


;;; yas/snippets
(after 'yasnippet
       (require 'dropdown-list)
       (yas/load-directory
	(format "%ssnippets/" (file-name-directory (locate-library "yasnippet"))))
       (yas/load-directory "~/.emacs.d/snippets/")
       (setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt yas/x-prompt yas/no-prompt)))

(after 'yasnippet-autoloads
       (add-hook 'markdown-mode-hook 'yas/minor-mode)
       (add-hook 'prog-mode-hook 'yas/minor-mode))

;;; expand-region
(global-set-key (kbd "C-@") 'er/expand-region)

;;; jump-char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)


;;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; mark-multiple
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(global-set-key (kbd "C-S-c C-S-c") 'mc/add-multiple-cursors-to-region-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)


;;; rainbow-delimiters
(after 'rainbow-delimiters-autoloads
       (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))


;;; undo-tree
(after 'undo-tree-autoloads
       (global-undo-tree-mode t)
       (setq undo-tree-auto-save-history t)
       (setq undo-tree-visualizer-relative-timestamps t)
       (setq undo-tree-visualizer-timestamps t))



;;; evil-mode
(after 'evil-autoloads
       (autoload 'evil-mode "evil" "EVIL MODE! RARR" t))

(after 'evil
       (when (boundp 'global-surround-mode) (global-surround-mode))
       (ignore-errors (require 'evil-leader)))

;;; evil-leader
(after 'evil-leader
       (evil-leader/set-key (kbd "t") 'find-file-in-project))


;;; auctex
(after 'latex
       (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
       (add-hook 'LaTeX-mode-hook 'variable-pitch-mode)
       (add-hook 'LaTeX-mode-hook 'hl-sentence-mode)
       (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)

       (setq TeX-source-correlate-method 'synctex)
       (setq TeX-auto-save t)
       (setq TeX-parse-self t)
       (setq TeX-save-query nil)
       (setq TeX-item-indent 0)
       (setq TeX-newline-function 'reindent-then-newline-and-indent)
       (setq-default TeX-PDF-mode t)
       ;; (setq-default TeX-master nil)
       ;; (setq LaTeX-command "latex")
       (setq TeX-view-program-list
	     '(("Skim"
		"/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
       (setq TeX-view-program-selection '((output-pdf "Skim")))


       ;; (add-hook 'LaTeX-mode-hook 'longlines-mode)
       (add-hook 'LaTeX-mode-hook 'flyspell-mode)
       (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
       (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
       (setq reftex-plug-into-AUCTeX t)
       (define-key TeX-mode-map (kbd "C-c C-m") 'TeX-command-master)
       (define-key TeX-mode-map (kbd "C-c C-c") 'TeX-compile))

(after 'reftex
       (add-to-list 'reftex-section-prefixes '(1 . "chap:")))


;;; magit
(global-set-key (kbd "C-x g") 'magit-status)



;;; deft
(global-set-key (kbd "C-c d") 'deft)


(after 'deft
       (setq deft-directory "/Users/dcurtis/Dropbox/Elements")
       (setq deft-text-mode 'markdown-mode)
       (setq deft-use-filename-as-title t))


;;; markdown
(setq auto-mode-alist
      (cons '("\\.te?xt\\'" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm?d\\'" . markdown-mode) auto-mode-alist))
(setq markdown-command "pandoc -S")
(setq markdown-latex-command
      "pandoc --template=$HOME/Coe/templates/pandocnarrow.tex -s -t latex -Vfontsize:10pt")

(after 'markdown-mode
       (remove-hook 'text-mode-hook 'turn-on-auto-fill)
       (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab)
       (define-key markdown-mode-map (kbd "C-c r") 'markdown-copy-rtf)
       (define-key markdown-mode-map (kbd "C-c l") 'markdown-export-latex)
       (define-key markdown-mode-map (kbd "C-c v") 'marked)
       (define-key markdown-mode-map (kbd "C-c c") 'markdown-copy-html)
       (define-key markdown-mode-map (kbd "C-c p") 'markdown-export-pdf)
       (define-key markdown-mode-map (kbd "C-c s") 'markdown-copy-paste-html)
       (define-key markdown-mode-map (kbd "C-c =") 'markdown-cleanup-list-numbers))

(add-hook 'markdown-mode-hook 'abbrev-mode)


;;; prog-mode
(defun mp-buffer-enable-whitespace-cleanup ()
  "enable whitespace-cleanup in the current buffer"
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(add-hook 'prog-mode-hook 'mp-buffer-enable-whitespace-cleanup)
(add-hook 'prog-mode-hook 'whitespace-mode)


;;; emacs lisp
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
(add-font-lock-numbers 'emacs-lisp-mode)

(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)


;;; paredit
(after 'paredit-autoloads (add-hook 'emacs-lisp-mode-hook 'paredit-mode))


;;; c / c++ mode
(setq c-cleanup-list '(defun-close-semi
			list-close-comma
			scope-operator
			compact-empty-funcall
			comment-close-slash))
(setq c-default-style '((c-mode . "cc-mode")
			(c++-mode . "cc-mode")
			(java-mode . "java")
			(awk-mode . "awk")
			(other . "gnu")))
(setq c-offsets-alist '((substatement-open . 0)))

(add-font-lock-numbers 'c-mode)
(add-font-lock-numbers 'c++-mode)

(defun mp-add-c-mode-bindings ()
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c C-m") 'compile-make))

(add-hook 'c-mode-common-hook 'mp-add-c-mode-bindings)

(if (< emacs-major-version 24)
    (mapc (lambda (h) (mapc (lambda (f) (add-hook h f)) prog-mode-hook))
	  '(c++-mode-hook c-mode-hook)))


;;; python
(add-font-lock-numbers 'python-mode)

(defun python-modes-init ()
  "initialization for all python modes"
  ;; (setup-virtualenv)
  ;; (define-key python-mode-map (kbd "C-h")
  ;; 'python-indent-dedent-line-backspace

  (push "~/.virtualenvs/default/bin" exec-path)
  (setenv "PATH"
	  (concat
	   "~/.virtualenvs/default/bin" ":"
	   (getenv "PATH")
	   ))

  (font-lock-add-keywords 'python-mode
			  `((,(rx symbol-start (or "import" "from")
				  symbol-end) 0 font-lock-preprocessor-face)))

  (make-face 'font-lock-operator-face)
  (set-face-attribute
   'font-lock-operator-face nil :inherit font-lock-keyword-face)
  (setq font-lock-operator-face 'font-lock-operator-face)
  (font-lock-add-keywords
   'python-mode
   `((,(rx symbol-start (or "in" "and" "or" "is" "not") symbol-end)
      0 font-lock-operator-face)))

  (add-font-lock-numbers 'python-mode)
  (font-lock-add-keywords
   'python-mode
   `(("^[       ]*\\(@\\)\\([a-zA-Z_][a-zA-Z_0-9.]+\\)\\((.+)\\)?"
      (1 'font-lock-preprocessor-face)
      (2 'font-lock-builtin-face))))
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error))

(after 'python-mode (python-modes-init))

;;;; pyflakes
(defun mp-flymake-pyflakes-init (&optional trigger-type)
  ;; Make sure it's not a remote buffer or flymake would not work
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-with-folder-structure))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "/Users/dcurtis/.virtualenv/bin/pyflakes" (list temp-file))))

(after 'flymake
       (add-to-list 'flymake-allowed-file-name-masks
		    '("\\.py\\'" mp-flymake-pyflakes-init)))



;;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;;; ruby
(after 'ruby-mode
       (add-hook 'ruby-mode-hook 'run-prog-mode-hook))

(after 'rvm-autoloads
       (add-hook 'ruby-mode-hook 'rvm-use-default))

(after 'find-file-in-project
       (add-to-list 'ffip-patterns "*.c")
       (add-to-list 'ffip-patterns "*.css")
       (add-to-list 'ffip-patterns "*.h"))


;;; html
(add-font-lock-numbers 'html-mode)


;;; php
(setq auto-mode-alist
      (cons '("\\.php[345]?\\'\\|\\.phtml\\." . php-mode) auto-mode-alist))


;;; mmm-mode
(after 'mmm-mode-autoloads
       (require 'mmm-auto)
       (setq mmm-global-mode 'maybe)
       (setq mmm-submode-decoration-level 2)
       (setq nxml-slash-auto-complete-flag t)
       (mmm-add-mode-ext-class 'nxml-mode "\\.php\\'" 'html-php)
       (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
       )


;;; nxhtml
(after 'nxhtml-autoloads
       (autoload 'django-html-mumamo-mode
	 (expand-file-name "autostart.el"
			   (file-name-directory (locate-library "nxhtml-autoloads"))))
       (setq auto-mode-alist
	     (append '(("\\.html?$" . django-html-mumamo-mode)) auto-mode-alist))
       (setq mumamo-background-colors nil)
       (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode)))


;;; pony-mode
(after 'pony-mode
       (setq pony-snippet-dir
	     (expand-file-name
	      "snippets/"
	      (file-name-directory (locate-library "pony-mode")))))


;;; ocatve-mode
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(setenv "GNUTERM" "x11")


;;; portability settings

;;;; aquamacs
(if (boundp 'aquamacs-version)
    (setq custom-file "~/.emacs.d/aqustom.el")
  (setq custom-file "~/.emacs.d/custom.el"))
(load custom-file)

(put 'narrow-to-region 'disabled nil)

(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))


;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
