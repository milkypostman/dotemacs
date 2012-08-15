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

;;; packages
;; ace-jump-mode
;; browse-kill-ring
;; clojure-mode
;; deft
;; dired+
;; expand-region
;; full-ack
;; ido-ubiquitous
;; jump-char
;; magit
;; mark-multiple
;; markdown-mode
;; markdown-mode+
;; mmm-mode
;; multiple-cursors
;; paredit
;; smex
;; undo-tree
;; virtualenv
;; yasnippet

;;; themes
;; color-theme-sanityinc-tomorrow
;; ir-black-theme
;; ir_black-theme
;; pastels-on-dark-theme
;; tango-2-theme
;; twilight-anti-bright
;; twilight-bright
;; twilight-theme
;; zen-and-art-theme

;; external libraries
(require 'checkdoc)
(require 'midnight)
(require 'misc)
(require 'recentf)
(require 'saveplace)
(require 'uniquify)



;;; functions
(require 'defun)

(setq mp-extra-paths
      '("~/.virtualenv/default/bin/"
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
(global-set-key (kbd "C-x O") 'other-window-reverse)
(global-set-key (kbd "M-`") 'other-frame)

(global-set-key (kbd "C-4") 'ctl-x-4-prefix)

(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)

(global-set-key (kbd "M-RET") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c r") 'iterm-run-previous-command)


(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-h") (kbd "<DEL>"))

(global-set-key (kbd "C-c h") 'help-command)

(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)

(global-set-key (kbd "C-c w") 'copy-paste)

(global-set-key (kbd "C-x C-i") 'imenu)

(global-set-key (kbd "C-.") 'hippie-expand)
(global-set-key (kbd "C-:") 'hippie-expand-line)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-M-/") 'hippie-expand-line)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)





(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key 'help-command "a" 'apropos)


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
(fringe-mode 1)
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
(setq-default
 ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]
 ansi-color-for-comint-mode t
 ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
 auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosave/" t)))
 backup-directory-alist (quote (("." . "~/.emacs.d/backups/")))
 undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/")))
 cua-enable-cua-keys nil
 custom-theme-directory "~/.emacs.d/themes/"
 delete-auto-save-files nil
 diff-switches "-u"
 enable-recursive-minibuffers t
 flymake-gui-warnings-enabled t
 ibuffer-expert t
 ibuffer-show-empty-filter-groups nil
 imenu-auto-rescan t
 indent-tabs-mode nil
 indicate-empty-lines t
 inhibit-startup-echo-area-message "dcurtis"
 inhibit-startup-screen t
 ispell-extra-args (quote ("--sug-mode=ultra"))
 ispell-program-name "aspell"
 line-spacing 2
 mode-line-in-non-selected-windows t
 mode-line-inverse-video t
 mouse-wheel-scroll-amount (quote (0.01))
 mouse-yank-at-point t
 ns-alternate-modifier (quote super)
 ns-command-modifier (quote meta)
 ns-pop-up-frames nil
 ns-right-alternate-modifier (quote alt)
 ns-tool-bar-display-mode 'both
 ns-tool-bar-size-mode 'regular
 redisplay-dont-pause t
 ring-bell-function 'ignore
 save-place t
 save-place-file "~/.emacs.d/places"
 scroll-conservatively 5
 scroll-margin 5
 send-mail-function (quote mailclient-send-it)
 sentence-end-double-space nil
 set-mark-command-repeat-pop t
 shift-select-mode nil
 split-height-threshold nil
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)"
 tramp-remote-path '(tramp-default-remote-path tramp-own-remote-path "/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")
 truncate-lines t
 uniquify-buffer-name-style 'reverse
 uniquify-ignore-buffers-re "^\\*"
 uniquify-separator " • "
 user-full-name "Donald Ephraim Curtis"
 user-mail-address "dcurtis@milkbox.net"
 visible-bell nil
 whitespace-style '(face trailing lines-tail space-before-tab indentation space-after-tab))

;;;; Darwin specific
(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")
       (setenv
        "PYTHONPATH"
        "/Users/dcurtis/src/compepi:/Users/dcurtis/src/networkx")))

;;;; GUI settings
(when window-system
  (menu-bar-mode t)
  (set-face-attribute 'default nil :font "Inconsolata-13")
  ;; specify a unicode font : MENLO (forced normal)
  (set-fontset-font "fontset-default" 'unicode "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))

;;; hippie-expand
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))



;;; dired
(global-set-key (kbd "C-x C-j") 'dired-jump)
(define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)
(global-set-key (kbd "M-s f") 'find-name-dired)

(after 'dired
       (after 'dired+-autoloads)
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
(setq ido-auto-merge-work-directories-length nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-prospects 10)
(setq ido-read-file-name-non-ido nil)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-virtual-buffers t)

(global-set-key (kbd "C-x f") 'find-file-in-project)
(define-key ctl-x-4-map (kbd "f") 'find-file-in-project-other-window)
(define-key ctl-x-4-map (kbd "s") 'shell-other-window)

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
(after 'ido-ubiquitous-autoloads (ido-ubiquitous-mode t))
(after 'ido-ubiquitous (ido-ubiquitous-disable-in evil-ex))

(setq ido-ubiquitous-command-exceptions '(evil-ex execute-extended-command))
(setq ido-ubiquitous-function-exceptions '(grep-read-files ucs-insert))



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
       (yas/reload-all)
       (setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt yas/x-prompt yas/no-prompt)))

(after 'yasnippet-autoloads
       (add-hook 'markdown-mode-hook 'yas/minor-mode)
       (add-hook 'prog-mode-hook 'yas/minor-mode))

;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;;; jump-char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)


;;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; mark-multiple
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)


;;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)


;;; rainbow-delimiters
(after 'rainbow-delimiters-autoloads
       (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))


;;; undo-tree
(after 'undo-tree-autoloads
       (global-undo-tree-mode t)
       (when (version-list-<= '(24 1 50 1) (version-to-list emacs-version) )
         (setq undo-tree-auto-save-history t))
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


;;; hl-sentence
(after 'hl-sentence-autoloads
       (add-hook 'LaTeX-mode-hook 'hl-sentence-mode))


;;; auctex
(after 'latex
       (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
       (add-hook 'LaTeX-mode-hook 'variable-pitch-mode)
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
       (define-key TeX-mode-map (kbd "C-M-h") 'mark-paragraph)
       (define-key TeX-mode-map (kbd "C-c C-m") 'TeX-command-master)
       (define-key TeX-mode-map (kbd "C-c C-c") 'TeX-compile))

(after 'reftex
       (add-to-list 'reftex-section-prefixes '(1 . "chap:")))


;;; magit
(global-set-key (kbd "C-x g") 'magit-status)



;;; deft
(global-set-key (kbd "C-c d") 'deft)


(after 'deft
       (setq deft-directory "/Users/dcurtis/Dropbox/notes")
       (setq deft-text-mode 'markdown-mode)
       (setq deft-use-filename-as-title t))


;;; markdown
(setq auto-mode-alist
      (cons '("\\.te?xt\\'" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm?d\\'" . markdown-mode) auto-mode-alist))
(setq markdown-command "pandoc -S")
(setq markdown-latex-command
      "pandoc --template=$HOME/Coe/templates/pandocwide.tex -s -t latex -Vfontsize:10pt")

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
(add-hook 'markdown-mode-hook 'toggle-truncate-lines)
(add-hook 'markdown-mode-hook 'toggle-word-wrap)


;;; prog-mode
(defun mp-buffer-enable-whitespace-cleanup ()
  "enable whitespace-cleanup in the current buffer"
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(add-hook 'prog-mode-hook 'mp-buffer-enable-whitespace-cleanup)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)


;;; emacs lisp
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
(add-font-lock-numbers 'emacs-lisp-mode)

(defun mp-buffer-enable-reindent ()
  "enable indent-buffer on the current buffer"
  (add-hook 'before-save-hook 'indent-buffer nil t))
(add-hook 'emacs-lisp-mode-hook 'mp-buffer-enable-reindent)
(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)


;;; paredit
(after 'paredit-autoloads (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
(after 'paredit-autoloads (add-hook 'clojure-mode-hook 'paredit-mode))


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
       (mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
       (mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'embedded-css)
       ;; (setq mmm-submode-decoration-level 2)
       ;; (setq nxml-slash-auto-complete-flag t)
       ;; (mmm-add-mode-ext-class 'nxml-mode "\\.php\\'" 'html-php)
       ;; (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
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
(put 'downcase-region 'disabled nil)


(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))


;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:

