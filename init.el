;;; init.el -- Milkmacs configuration file
;;
;; based on emacs-starter-kit
;;

(require 'cl)

;; load my functions
(add-to-list 'load-path "~/.emacs.d/")
(require 'defun)

;; emacs23 --
;; http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el

(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
;; (add-to-list 'package-archives
;;              '("khealy" . "http://kieranhealy.org/packages/") t)
;; (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpamirror" . "http://milkypostman.github.com/melpa/packages/") t)
;; (setq url-http-attempt-keepalives nil) ;; temporary hack until I can get hosting fixed.
(package-initialize)

;; debug if we would like
;; (setq debug-on-error t)

(require 'uniquify)
(require 'midnight)
(require 'misc)

(require 'checkdoc)


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



(ignore-errors
  (server-start))

;; the uncustomizable
(setq-default cursor-type '(bar . 2))

(setq ring-bell-function 'ignore)
(setq redisplay-dont-pause t)

;; ispell
(setq ispell-list-command "list")


(defalias 'wq 'save-buffers-kill-emacs)
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'eshell/ff 'find-file)
(defalias 'eshell/ffow 'find-file-other-window)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)



;; make <C-tab> be M-TAB
;; (define-key function-key-map (kbd "<C-tab>") (kbd "M-TAB"))
;; (global-set-key (kbd "<C-tab>") (kbd "M-TAB"))
(define-key key-translation-map (kbd "<C-tab>") (kbd "M-TAB"))

(global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-M-SPC") 'just-one-space)
(global-set-key (kbd "A-h") 'ns-do-hide-emacs)
(global-set-key (kbd "A-M-h") 'ns-do-hide-others)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-M-h") 'mark-defun)
(global-set-key (kbd "C-M-,") 'beginning-of-buffer-other-window)
(global-set-key (kbd "M-`") 'other-frame)

(global-set-key (kbd "C-4") 'ctl-x-4-prefix)
(global-set-key (kbd "C-.") 'repeat)

(global-set-key (kbd "M-z") 'zap-to-char)

(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)

(global-set-key (kbd "M-C-m") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-c") 'close-frame-or-client)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c r") 'iterm-run-previous-command)

(define-key ctl-x-4-map "f" 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-d") 'ido-dired)

(global-set-key (kbd "C-c d") 'deft)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-w") (kbd "M-<DEL>"))
(global-set-key (kbd "C-h") (kbd "<DEL>"))

(global-set-key (kbd "C-c h") 'help-command)

(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)

(global-set-key (kbd "C-c w") 'mp-copy-paste)

(define-key key-translation-map (kbd "C-x C-m") (kbd "M-x"))


(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))


(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)
(add-hook 'ido-setup-hook 'mp-ido-hook)


;;; Markdown
(setq auto-mode-alist
      (cons '("\\.te?xt\\'" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm?d\\'" . markdown-mode) auto-mode-alist))


;; (eval-after-load 'python '(python-modes-init))
(eval-after-load 'python-mode '(python-modes-init))

;; yas/snippets
(eval-after-load 'yasnippet
  '(progn
     (require 'dropdown-list)
     (yas/load-directory
      (format "%ssnippets/" (file-name-directory (locate-library "yasnippet"))))
     (yas/load-directory "~/.emacs.d/snippets/")
     ))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

(eval-after-load 'yasnippet-autoloads
  '(progn
     (add-hook 'markdown-mode-hook 'yas/minor-mode)
     (add-hook 'prog-mode-hook 'yas/minor-mode)))

(eval-after-load 'expand-region
  '(progn 'er/expand-region
          (global-set-key (kbd "C-@") 'er/expand-region)))

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'markdown-mode-hook 'mp-turn-on-abbrev-mode)

(setq auto-mode-alist
      (cons '("\\.php[345]?\\'\\|\\.phtml\\." . php-mode) auto-mode-alist))

(eval-after-load 'mmm-mode-autoloads
  '(progn
     (require 'mmm-auto)
     (setq mmm-global-mode 'maybe)
     (setq mmm-submode-decoration-level 2)
     (setq nxml-slash-auto-complete-flag t)
     (mmm-add-mode-ext-class 'nxml-mode "\\.php\\'" 'html-php)
     (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
     ))

(eval-after-load 'rainbow-delimiters-autoloads
  '(progn
     (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)))

(eval-after-load 'markdown-mode
  '(progn
     (remove-hook 'text-mode-hook 'turn-on-auto-fill)
     (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab)
     (define-key markdown-mode-map (kbd "C-c r") 'markdown-copy-rtf)
     (define-key markdown-mode-map (kbd "C-c l") 'markdown-export-latex)
     (define-key markdown-mode-map (kbd "C-c v") 'marked)
     (define-key markdown-mode-map (kbd "C-c c") 'markdown-copy-html)
     (define-key markdown-mode-map (kbd "C-c p") 'markdown-export-pdf)
     (define-key markdown-mode-map (kbd "C-c s") 'markdown-copy-paste-html)
     (define-key markdown-mode-map (kbd "C-c =") 'markdown-cleanup-list-numbers)))


;; ocatve-mode
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; pony-mode
(eval-after-load 'pony-mode
  '(progn
     (setq pony-snippet-dir
           (expand-file-name
            "snippets/"
            (file-name-directory (locate-library "pony-mode"))))))


;; auctex
(eval-after-load 'latex
  '(progn
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
     (define-key TeX-mode-map (kbd "C-c C-c") 'TeX-compile)))

(eval-after-load 'reftex
  '(progn
     (add-to-list 'reftex-section-prefixes '(1 . "chap:"))))

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


(add-hook 'ibuffer-mode-hook 'mp-ibuffer-hook)
(add-hook 'write-file-functions 'time-stamp)

(add-hook 'c-mode-common-hook 'mp-add-c-mode-bindings)

;; paredit
;; (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(eval-after-load 'starter-kit-autoloads
  '(progn
     (add-hook 'prog-mode-hook 'whitespace-mode)
     (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(add-hook 'prog-mode-hook 'mp-buffer-enable-whitespace-cleanup)

(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook 'mp-run-prog-mode-hook)))

(eval-after-load 'rvm-autoloads
  '(progn
     (add-hook 'ruby-mode-hook 'rvm-use-default)))

(eval-after-load 'find-file-in-project
  '(progn
     (add-to-list 'ffip-patterns "*.c")
     (add-to-list 'ffip-patterns "*.h")))

(eval-after-load 'flymake
  '(progn
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" mp-flymake-pyflakes-init))))


;; (font-lock-add-keywords 'emacs-lisp-mode
;; '(("(\\|)\\|'" . 'font-lock-exit-face)))
;; (font-lock-add-keywords 'emacs-lisp-mode
;; '(("add-to-list" . font-lock-keyword-face)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
(add-font-lock-numbers 'c-mode)
(add-font-lock-numbers 'c++-mode)
(add-font-lock-numbers 'html-mode)
(add-font-lock-numbers 'emacs-lisp-mode)


;; Darwin Specific
(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")
       (setenv
        "PYTHONPATH"
        "/Users/dcurtis/src/compepi:/Users/dcurtis/src/networkx")))

(message "done with all but custom")

(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

(if (boundp 'aquamacs-version)
    (setq custom-file "~/.emacs.d/aqustom.el")
  (setq custom-file "~/.emacs.d/custom.el"))
(load custom-file)


(put 'narrow-to-region 'disabled nil)


(if (< emacs-major-version 24)
    (mapc (lambda (h) (mapc (lambda (f) (add-hook h f)) prog-mode-hook))
          '(c++-mode-hook c-mode-hook)))

(when window-system
  (menu-bar-mode t)
  ;; specify a unicode font : MENLO (forced normal)
  (set-fontset-font "fontset-default" 'unicode "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))



;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
