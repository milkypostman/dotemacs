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

(use-package misc
  :bind (("M-z" . zap-up-to-char)))

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

(use-package emacs
  :commands (indent-buffer buffer-enable-reindent buffer-disable-reindent)
  :config
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

(use-package ediff
  :hook (ediff-keymap-setup . add-d-to-ediff-mode-map)
  :commands add-d-to-ediff-mode-map
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  (defun ediff-write-merge-buffer ()
    (let ((file ediff-merge-store-file))
      (set-buffer ediff-buffer-C)
      (write-region (point-min) (point-max) file)
      (message "Merge buffer saved in %s" file)
      (set-buffer-modified-p nil)
      (sit-for 1)))
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)))

(use-package markdown-mode
  :ensure
  :bind (:map markdown-mode-map
              ("C-c C-r" . decurtis-markdown-insert-reference-link))
  :defer
  :config
  (defun decurtis-markdown-insert-reference-link ()
    "Insert reference links."
    (interactive)
    (let* ((defined-refs (markdown-get-defined-references))
           (ref (completing-read
                 "reference: "
                 defined-refs
                 nil nil nil)))
      (unless (member ref defined-refs)
        (let (uri)
          (cond ((thing-at-point-looking-at markdown-regex-uri)
                 (setq uri (match-string-no-properties 1))
                 (delete-region (match-beginning 0) (match-end 0)))
                ((string-match markdown-regex-uri (current-kill 0))
                 (setq uri (current-kill 0)))
                (t (setq uri (completing-read "URL: " nil))))
          (markdown-insert-reference-definition ref uri)))
      (insert (concat "[" ref "]"))))

  (defun decurtis-markdown-insert-inline-link ()
    "Insert inline links."
    (interactive)
    (let ((text (completing-read "text: " nil))
          uri)
      (cond ((thing-at-point-looking-at markdown-regex-uri)
             (setq uri (match-string-no-properties 1))
             (delete-region (match-beginning 0) (match-end 0)))
            ((string-match markdown-regex-uri (current-kill 0))
             (setq uri (current-kill 0)))
            (t (setq uri (completing-read "URL: " nil))))
      (insert (concat "[" text "](" uri ")")))))

(use-package elisp-mode
  :after emacs
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

(use-package go-mode
  :ensure
  :defer
  :functions decurtis-go-hook
  :hook (go-mode-hook . decurtis-go-hook)
  :config
  (defun decurtis-go-hook ()
    "Hooks for `go-mode'."
    (setq tab-width 2)
    (whitespace-mode -1)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

(use-package prog-mode
  :defer)

;; (use-package amx
;;   :ensure)

(use-package flx
  :ensure
  :after ivy)

(use-package ivy
  :ensure
  :diminish
  :commands (decurtis-ivy-arrow-prefix ivy-mode)
  :config
  (defun decurtis-ivy-arrow-prefix (cands)
    "Transform CAND-PAIRS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat "> " (ivy--add-face (concat str "\n") 'ivy-current-match)))
     (lambda (str)
       (concat "  " str "\n"))
     cands
     ""))
  (ido-mode -20))

(use-package ivy-prescient
  :ensure)

(use-package midnight
  :ensure
  :init (setq midnight-period 3600))

(use-package counsel
  :ensure
  :defer
  :diminish
  :config (setq ivy-initial-inputs-alist nil))

(use-package swiper
  :ensure
  :bind (("C-c s" . swiper))
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)
                                (swiper . ivy--regex))))

(use-package diff-hl
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

(use-package evil-anzu
  :after evil
  :ensure
  :diminish anzu-mode)

(use-package evil-leader
  :after evil
  :defer
  :ensure
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-key "SPC" 'counsel-M-x)

  (evil-leader/set-key ", SPC" 'avy-goto-word-1)
  (evil-leader/set-key ",/" 'avy-isearch)
  (evil-leader/set-key ",B" 'avy-goto-word-1-above)
  (evil-leader/set-key ",S" 'avy-goto-symbol-1)
  (evil-leader/set-key ",W" 'avy-goto-word-1-below)
  (evil-leader/set-key ",b" 'avy-goto-word-0-above)
  (evil-leader/set-key ",c" 'avy-goto-char)
  (evil-leader/set-key ",f" 'avy-goto-char-in-line)
  (evil-leader/set-key ",h" 'avy-goto-char-2)
  (evil-leader/set-key ",l" 'avy-goto-line)
  (evil-leader/set-key ",r" 'avy-resume)
  (evil-leader/set-key ",s" 'avy-goto-symbol-0)
  (evil-leader/set-key ",w" 'avy-goto-word-0-below)

  (evil-leader/set-key "b" 'ivy-switch-buffer)

  (evil-leader/set-key "db" 'gud-break)
  (evil-leader/set-key "dr" 'gud-remove)

  (evil-leader/set-key "e" 'eglot-code-actions)

  (evil-leader/set-key "f" 'counsel-find-file)

  (evil-leader/set-key "gg" 'magit-status)

  (evil-leader/set-key "hf" 'counsel-describe-function)
  (evil-leader/set-key "hn" 'highlight-symbol-next)
  (evil-leader/set-key "hp" 'highlight-symbol-prev)
  (evil-leader/set-key "hv" 'counsel-describe-variable)

  (evil-leader/set-key "i" 'clang-include-fixer-at-point)

  (evil-leader/set-key "mm" 'counsel-bookmark)
  (evil-leader/set-key "ms" 'bookmark-set)

  (evil-leader/set-key "n" 'next-error)
  (evil-leader/set-key "N" 'previous-error)
  (evil-leader/set-key "p" 'projectile-find-file)

  (evil-leader/set-key "sc" 'string-inflection-camelcase)

  (evil-leader/set-key "sU" 'string-inflection-upcase)
  (evil-leader/set-key "ska" 'smerge-keep-all)
  (evil-leader/set-key "skb" 'smerge-keep-base)
  (evil-leader/set-key "skc" 'smerge-keep-current)
  (evil-leader/set-key "skl" 'smerge-keep-lower)
  (evil-leader/set-key "skm" 'smerge-keep-mine)
  (evil-leader/set-key "sko" 'smerge-keep-other)
  (evil-leader/set-key "sku" 'smerge-keep-upper)
  (evil-leader/set-key "sn" 'smerge-next)
  (evil-leader/set-key "ss" 'string-inflection-all-cycle)
  (evil-leader/set-key "su" 'string-inflection-underscore)

  (evil-leader/set-key "t" 'projectile-find-file-dwim)

  (evil-leader/set-key "xa" 'xref-find-apropos)
  (evil-leader/set-key "xd" 'xref-find-definitions)
  (evil-leader/set-key "xo" 'xref-find-definitions-other-window)
  (evil-leader/set-key "xr" 'xref-find-references)
  (evil-leader/set-key "xx" 'xref-find-references-same-window)

  (evil-leader/set-key "y" 'bury-buffer)

  (evil-leader/set-key "Z" 'flymake-goto-prev-error)
  (evil-leader/set-key "z" 'flymake-goto-next-error))

(use-package magit
  :defer
  :ensure)

(use-package evil-surround
  :after evil
  :defer
  :ensure
  ;; initialize evil-surround-mode lazily otherwise it messes up other custom
  ;; variables.
  :init
  (global-evil-surround-mode t))

(use-package evil-multiedit
  :diminish
  :ensure
  :after evil
  :commands (evil-multiedit-default-keybinds)
  :init
  (evil-multiedit-default-keybinds))

(use-package evil-mc
  :diminish
  :ensure
  :after evil
  :bind ("M-<mouse-1>" . evil-mc-toggle-cursor-on-click)
  :config
  (add-to-list 'evil-mc-known-commands
               '(string-inflection-camelcase . ((:default . evil-mc-execute-default-call))))
  (add-to-list 'evil-mc-known-commands
               '(string-inflection-underscore . ((:default . evil-mc-execute-default-call))))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (defvar evil-mc-key-map
    (let ((map (make-sparse-keymap))
          (keys '(("grm" . evil-mc-make-all-cursors)
                  ("gru" . evil-mc-undo-last-added-cursor)
                  ("grq" . evil-mc-undo-all-cursors)
                  ("grs" . evil-mc-pause-cursors)
                  ("grr" . evil-mc-resume-cursors)
                  ("grf" . evil-mc-make-and-goto-first-cursor)
                  ("grl" . evil-mc-make-and-goto-last-cursor)
                  ("grh" . evil-mc-make-cursor-here)
                  ("grj" . evil-mc-make-cursor-move-next-line)
                  ("grk" . evil-mc-make-cursor-move-prev-line)
                  ("M-n" . evil-mc-make-and-goto-next-cursor)
                  ("grN" . evil-mc-skip-and-goto-next-cursor)
                  ("M-p" . evil-mc-make-and-goto-prev-cursor)
                  ("grP" . evil-mc-skip-and-goto-prev-cursor)
                  ("C-n" . evil-mc-make-and-goto-next-match)
                  ("grn" . evil-mc-skip-and-goto-next-match)
                  ("C-t" . evil-mc-skip-and-goto-next-match)
                  ("C-p" . evil-mc-make-and-goto-prev-match)
                  ("grp" . evil-mc-skip-and-goto-prev-match)))
          (visual-keys
           '(("gri" . evil-mc-make-cursor-in-visual-selection-beg)
             ("gre" . evil-mc-make-cursor-in-visual-selection-end))))
      (dolist (key-data keys)
        (evil-define-key 'normal map (kbd (car key-data)) (cdr key-data))
        (evil-define-key 'visual map (kbd (car key-data)) (cdr key-data)))
      (dolist (key-data visual-keys)
        (evil-define-key 'visual map (kbd (car key-data)) (cdr key-data)))
      map)
    "Overwrite evil-mc keymap.")
  (global-evil-mc-mode))

(use-package avy
  :ensure
  :bind ("C-c C-SPC" . avy-goto-word-1)
  :commands (avy-goto-word-0-below avy-goto-word-0-above)
  :config
  (defun avy-goto-symbol-0 (arg &optional beg end)
    "Jump to a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
    (interactive "P")
    (avy-with avy-goto-symbol-0
      (avy--generic-jump "\\_<\\(\\sw\\|\\s_\\)" arg avy-style beg end))))

(use-package ace-jump-buffer
  :ensure
  :bind (("C-c C-j" . ace-jump-buffer)))

(use-package autorevert
  :ensure
  :diminish global-auto-revert-mode)

(use-package company
  :diminish
  :ensure
  :bind (:map company-active-map
              ("C-h" . "")
              ("C-c h" . company-show-doc-buffer)
              ("C-w" . kill-region-or-backward-word)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("S-TAB" . company-select-previous)
              ("<backtab>" . company-select-previous))
  :config
  (defun company-disable ()
    "Unconditionally disable company-mode in the current buffer."
    (company-mode nil)))

(use-package company-flx
  :after company
  :defer
  :ensure
  :config (company-flx-mode +1))

(use-package expand-region
  :ensure
  :bind (("C-c w" . er/expand-region)))

(use-package ag
  :ensure
  :defer
  :config
  (define-key ag-mode-map (kbd "k") nil))

(use-package fish-mode
  :ensure
  :defer
  :config
  (defun decurtis-fish-hook ()
    "Hooks for `fish-mode'."
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (add-hook 'before-save-hook 'fish_indent-before-save))
  (add-hook 'fish-mode-hook #'decurtis-fish-hook))

(use-package wgrep-ag
  :commands wgrep-change-to-wgrep-mode
  :ensure
  :defer
  :config
  (defun wgrep-ag-prepare-header/footer ()
    (save-excursion
      (goto-char (point-min))
      ;; Look for the first useful result line.
      (if (re-search-forward (concat wgrep-ag-grouped-result-file-regexp
                                     "\\|"
                                     wgrep-ag-ungrouped-result-regexp))
          (add-text-properties (point-min) (line-beginning-position)
                               '(read-only t wgrep-header t))
        ;; No results in this buffer, let's mark the whole thing as
        ;; header.
        (add-text-properties (point-min) (point-max)
                             '(read-only t wgrep-header t)))

      ;; OK, header dealt with. Now let's try find the footer.
      (goto-char (point-max))
      (re-search-backward "^\\(?:-[^:]+?:[[:digit:]]+:[[:digit:]]+:\\)" nil t)
      ;; Point is now at the beginning of the result nearest the end
      ;; of the buffer, AKA the last result.  Move to the start of
      ;; the line after the last result, and mark everything from
      ;; that line forward as wgrep-footer.  If we can't move to the
      ;; line after the last line then there apparently is no
      ;; footer.
      (when (zerop (forward-line 1))
        (add-text-properties (point) (point-max)
                             '(read-only t wgrep-footer t))))))

(use-package phi-search
  :ensure
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)
         ;;("M-%" . phi-replace-query)
         ))

(use-package multiple-cursors
  :ensure
  :init (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("C-c RET" . mc/mark-more-like-this-extended)
         ("C-c m c" . mc/edit-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m m" . mc/mark-more-like-this-extended)
         ("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c =" . mc/insert-numbers)
         ("C-c *" . mc/mark-all-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click)
         ;; :map mc/keymap
         ;; ("C-g" . mc/keyboard-quit)
         ))

(use-package string-inflection
  :ensure
  :defer
  :config (setq string-inflection-skip-backward-when-done t))

(use-package projectile
  :ensure
  :bind (("C-x f" . projectile-find-file-dwim)
         :map ctl-x-4-map
         ("f" . projectile-find-file-dwim-other-window)))

(use-package which-key
  :defer
  :diminish
  :ensure)

(use-package undo-tree
  :diminish
  :ensure
  :defer)


(use-package jsonnet-mode
  :ensure)

(use-package yasnippet
  :defer
  :ensure)


(use-package ivy-xref
  :ensure t
  ;; (setq xref-show-xrefs-function #'xref--show-xref-buffer)
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package highlight-symbol
  :ensure)

(use-package compilation-colorization
  :defer)

;;;; functions
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph or REGION and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun tmux-send-paragraph ()
  "Send the current paragraph to tmux using emamux."
  (interactive)
  (save-excursion
    (if (or current-prefix-arg (not (emamux:set-parameters-p)))
        (emamux:set-parameters))
    (mark-paragraph)
    (emamux:send-keys
     (s-trim-left
      (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)))

(defun align-second-column ()
  "Align the second space delimeted column."
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\w+\\(\\s-*\\)" 1 2 nil)
  (deactivate-mark))

(defun insert-deb-date ()
  "Insert the current date and time using the format required for debian files."
  (interactive)
  (shell-command "date +'%a, %d %b %Y %H:%m:%S %z'" t))

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (let ((kill-current-buffer-running t))
    (kill-buffer (current-buffer))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" filename)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'"
                        name (file-name-nondirectory new-name))))))))

(defun delete-current-buffer-file ()
  "Deletes current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name (buffer-base-buffer))))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" filename)
      (delete-file filename)
      (kill-this-buffer))))

(defun open-next-line (arg)
  "Move to the next line and open ARG line(s).

See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (newline-and-indent))

(defun open-previous-line (arg)
  "Open a ARG line(s) before the current one.

See also `newline-and-indent'."
  (interactive "p")
  (when (eolp)
    (save-excursion
      (delete-region (point)
                     (progn (skip-chars-backward " \t") (point)))))
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun what-face (pos)
  "Print the face at POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))


(defun duplicate-region (num &optional start end)
  "Duplicates NUM times the region bounded by START and END times.
If no START and END is provided, the current `region-beginning' and
`region-end' is used."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (i num)
      (insert region))))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))

(defun file-name-to-kill-ring ()
  "Copy the current buffer file name to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun split-window-right-equal (&optional size)
  "Split window to the right with SIZE and make all windows equal."
  (interactive)
  (split-window-right size)
  (when (not size) (balance-windows)))

(defun split-window-below-equal (&optional size)
  "Split window below to SIZE below and make all windows equal."
  (interactive)
  (split-window-below size)
  (when (not size) (balance-windows)))

(defun delete-window-equal (&optional window)
  "Delete the WINDOW and equalize all windows."
  (interactive)
  (delete-window window)
  (balance-windows))

(defun other-window-reverse ()
  "Go to `other-window' in reverse direction."
  (interactive)
  (other-window -1))

(defun toggle-window-split ()
  "Toggle between vertical and horizontal split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun shift-element (ele lst)
  "Cyclical shift ELE in LST to the right."
  (if (> (length lst) 1)
      (if (equal (car (last lst)) ele)
          (cons ele (butlast lst))
        (if (equal (car lst) ele)
            (cons (cadr lst) (cons ele (cddr lst)))
          (cons (car lst) (shift-element ele (cdr lst)))))
    lst))

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

(defun decurtis-align-table ()
  "Align a table. I guess."
  (interactive)
  (let ((beg (save-excursion (backward-paragraph) (point)))
        (end (save-excursion (forward-paragraph) (point))))

    (when (region-active-p)
      (setq beg (region-beginning))
      (setq end (region-end)))
    (message "%s %s" beg end)

    (align-regexp beg end "\\(\\s-*\\) " 1 1 t)))

(defun kill-region-or-backward-word (arg)
  "Kill region if active otherwise kill backward word ARG times."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (call-interactively (key-binding (kbd "M-<DEL>")) t (this-command-keys-vector))))

(defun window-enable-dedication ()
  "Enable dedicated state of window."
  (interactive)
  (set-window-dedicated-p (selected-window) t))

(defun window-disable-dedication ()
  "Disable dedicated state of window."
  (interactive)
  (set-window-dedicated-p (selected-window) nil))

(defun display-buffer-pop-up-frame-dedicated-window (buffer alist)
  "Call `display-buffer-pop-up-frame' and make the new window dedicated."
  (when (display-graphic-p (selected-frame))
    (let ((window (display-buffer-pop-up-frame buffer alist)))
      (when window
        (set-window-dedicated-p window t)))))


(put 'narrow-to-region 'disabled nil)


(setq frame-title-format
      '(("" invocation-name "@"
         (:eval (save-match-data
                  (string-match "^\\([^.]*\\).*" system-name)
                  (match-string 1 system-name))))
        (multiple-frames " - %b")
        (:eval (save-match-data
                 (unless (equal server-name "server")
                   (concat " - " server-name))))))


;;;; global key bindings
(global-set-key (kbd "C-'") #'jump-to-register)
(global-set-key (kbd "C-4") #'ctl-x-4-prefix)
(global-set-key (kbd "C-M-q") 'unfill-paragraph)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-\\") #'ctl-x-4-prefix)
(global-set-key (kbd "C-c M-x") #'execute-extended-command)
(global-set-key (kbd "C-c SPC") #'point-to-register)
(global-set-key (kbd "C-c d") #'duplicate-current-line-or-region)
(global-set-key (kbd "C-c c") #'compile)
(global-set-key (kbd "C-c f") #'decurtis-open-whatsout)
(global-set-key (kbd "C-c h") #'help-command)
(global-set-key (kbd "C-c k") #'kill-current-buffer)
(global-set-key (kbd "C-c y") #'bury-buffer)
(global-set-key (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-w") #'kill-region-or-backward-word)
(global-set-key (kbd "C-x -") #'toggle-window-split)
(global-set-key (kbd "C-x 0") #'delete-window-equal)
(global-set-key (kbd "C-x 2") #'split-window-below-equal)
(global-set-key (kbd "C-x 3") #'split-window-right-equal)
(global-set-key (kbd "C-x C--") #'rotate-windows)
(global-set-key (kbd "C-x C-_") #'rotate-windows)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-i") #'imenu)
(global-set-key (kbd "C-x r q") #'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-o") #'other-window)
(global-set-key (kbd "C-x C-S-o") #'other-window-reverse)
(global-set-key (kbd "<C-tab>") #'other-window)
(global-set-key (kbd "<C-iso-lefttab>") #'other-window-reverse)
(global-set-key (kbd "M-RET") #'open-next-line)
(global-set-key (kbd "M-i") #'back-to-indentation)
(global-set-key (kbd "M-o") #'open-previous-line)
(global-set-key (kbd "M-n") #'better-next-error)
(global-set-key (kbd "M-p") #'better-previous-error)
(global-set-key (kbd "RET") #'newline-and-indent)

(define-key key-translation-map (kbd "C-x C-m") (kbd "M-x"))
(define-key 'help-command "a" #'apropos)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)


(provide 'init)
;;; init.el ends here
