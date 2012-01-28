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
(setq url-http-attempt-keepalives nil) ;; temporary hack until I can get hosting fixed.
(package-initialize)

;; debug if we would like
;; (setq debug-on-error t)

(require 'uniquify)
(require 'midnight)
(require 'misc)

(push "~/.virtualenv/bin/" exec-path)
(push "~/.cabal/bin/" exec-path)
(push "~/bin/" exec-path)
(push "/usr/local/bin/" exec-path)
(push "/usr/texbin/" exec-path)
(setenv "PATH" (mapconcat 'identity
                          (delete-dups
                           (append
                            (list (concat (getenv "HOME") "/.virtualenv/bin")
                                  (concat (getenv "HOME") "/.cabal/bin")
                                  (concat (getenv "HOME") "/bin")
                                  "/usr/local/bin"
                                  "/usr/texbin")
                            (split-string (getenv "PATH") ":")))
                          ":"))




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

(global-set-key (kbd "C-@") 'er/expand-region)

(define-key key-translation-map (kbd "C-x C-m") (kbd "M-x"))


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

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(add-hook 'ido-setup-hook 'mp-ido-hook)

;; (setq auto-mode-alist
;;       (cons '("\\.text" . markdown-mode) auto-mode-alist))
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


;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'markdown-mode-hook 'mp-turn-on-abbrev-mode)

(eval-after-load 'rainbow-delimiters-autoloads
  '(progn
     (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)))

(eval-after-load 'markdown-mode
  '(progn
     (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab)
     (define-key markdown-mode-map (kbd "C-c r") 'markdown-copy-rtf)
     (define-key markdown-mode-map (kbd "C-c l") 'markdown-export-latex)
     (define-key markdown-mode-map (kbd "C-c v") 'marked)
     (define-key markdown-mode-map (kbd "C-c c") 'markdown-copy-html)
     (define-key markdown-mode-map (kbd "C-c p") 'markdown-export-pdf)
     (define-key markdown-mode-map (kbd "C-c s") 'markdown-copy-paste-html)
     (define-key markdown-mode-map (kbd "C-c =") 'markdown-cleanup-list-numbers)))


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


     (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     (setq reftex-plug-into-AUCTeX t)
     (define-key TeX-mode-map (kbd "C-c C-m") 'TeX-command-master)
     (define-key TeX-mode-map (kbd "C-c C-c") 'TeX-compile)
     ;; (define-key TeX-mode-map (kbd "C-c C-c")
     ;;   (lambda ()
     ;;     (interactive)
     ;;     (TeX-save-document (TeX-master-file))
     ;;     (TeX-command "LaTeX" 'TeX-master-file)
     ;;     ))
     ))

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
     (add-hook 'prog-mode-hook 'esk-turn-on-whitespace)
     (add-hook 'emacs-lisp-mode-hook 'esk-turn-on-paredit)))

(add-hook 'prog-mode-hook 'mp-buffer-enable-whitespace-cleanup)

(eval-after-load 'find-file-in-project
  '(progn
     (add-to-list 'ffip-patterns "*.c")
     (add-to-list 'ffip-patterns "*.h")))

;; faces
(make-face 'font-lock-number-face)
(set-face-attribute 'font-lock-number-face nil :inherit font-lock-constant-face)
(setq font-lock-number-face 'font-lock-number-face)
(defvar font-lock-number "[0-9-.]+\\([eE][+-]?[0-9]*\\)?")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
(defun add-font-lock-numbers (mode)
  (font-lock-add-keywords
   mode
   `((,(concat "\\<\\(" font-lock-number "\\)\\>" ) 0 font-lock-number-face)
     (,(concat "\\<\\(" font-lock-hexnumber "\\)\\>" ) 0 font-lock-number-face)
     )))


;; (font-lock-add-keywords 'emacs-lisp-mode
;; '(("(\\|)\\|'" . 'font-lock-exit-face)))
;; (font-lock-add-keywords 'emacs-lisp-mode
;; '(("add-to-list" . font-lock-keyword-face)))

;; (font-lock-add-keywords
;;  'emacs-lisp-mode
;;  '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
(add-font-lock-numbers 'emacs-lisp-mode)
(add-font-lock-numbers 'c-mode)
(add-font-lock-numbers 'c++-mode)


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

;; specify a fallback font : MENLO
(set-fontset-font "fontset-default" 'unicode "Menlo")


;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
