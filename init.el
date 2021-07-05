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
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("gnu-devel" . "https://elpa.gnu.org/devel/")
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

;;;; begin packages
(use-package emacs
  :commands (swap-windows
	     rotate-windows
	     indent-buffer
	     buffer-enable-reindent
	     buffer-disable-reindent)
  :bind (("C-x C--" . rotate-windows)
	 ("C-x C-i" . imenu)
	 ("C-x r q" . save-buffers-kill-emacs)
	 ("C-w" . kill-region-or-backward-word))
  :config
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

(use-package elisp-mode
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

(use-package magit
  :defer
  :ensure)

(use-package evil
  :ensure
  :bind (:map evil-normal-state-map
	      ("M-y" . consult-yank-from-kill-ring)
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

(use-package undo-fu
  :ensure)

(use-package evil-leader
  :after evil
  :defer
  :ensure
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-key "y" 'bury-buffer)
  (evil-leader/set-key "n" 'next-error)
  (evil-leader/set-key "gg" 'magit-status)
  (evil-leader/set-key "b" 'switch-to-buffer)
  (evil-leader/set-key "q" 'query-replace-regexp)
  (evil-leader/set-key "rg" 'rg)
  (evil-leader/set-key "f" 'find-file)
  )


;; (use-package selectrum
;;   :ensure
;;   :bind (:map selectrum-minibuffer-map
;;	      ("C-p" . selectrum-next-candidate)
;;	      ("C-n" . selectrum-previous-candidate)
;;	      ("C-w" . selectrum-backward-kill-sexp))
;;   :config
;;   (defun selectrum--vertical-display-style
;;       (win input nrows _ncols index
;;	   max-index _first-index-displayed _last-index-displayed)
;;     "Insert candidates vertically into current buffer.
;; Used as insertion function for `vertical' display style, see
;; `selectrum-display-style'. WIN is the window where buffer will get
;; displayed in. INPUT is the input string used to highlight the
;; candidates. NROWS is the number of lines available and NCOLS the
;; number of available columns. If there are candidates INDEX is the
;; index of the currently selected candidate and MAX-INDEX is the index
;; of the maximal index of the collection. When candidates are already
;; displayed FIRST-INDEX-DISPLAYED is the index of the candidate that is
;; displayed first and LAST-INDEX-DISPLAYED the index of the last one."
;;     (let* ((first-index-displayed
;;	    (if (not index)
;;		0
;;	      (selectrum--clamp
;;	       ;; Adding one here makes it look slightly better, as
;;	       ;; there are guaranteed to be more candidates shown
;;	       ;; below the selection than above.
;;	       (1+ (- index (max 1 (/ nrows 2))))
;;	       0
;;	       (max (- (1+ max-index) nrows)
;;		    0))))
;;	   (i first-index-displayed)
;;	   (highlighted-candidates
;;	    (selectrum--highlighted-candidates
;;	     input
;;	     first-index-displayed nrows))
;;	   (fill-rows (max 0 (- nrows (length highlighted-candidates))))
;;	   (metadata (selectrum--metadata))
;;	   (annotf (or (completion-metadata-get metadata 'annotation-function)
;;		       (plist-get completion-extra-properties
;;				  :annotation-function)))
;;	   (aff (or (completion-metadata-get metadata 'affixation-function)
;;		    (plist-get completion-extra-properties
;;			       :affixation-function)))
;;	   (docsigf (plist-get completion-extra-properties :company-docsig))
;;	   (groupf (and selectrum-group-format
;;			(completion-metadata-get metadata 'group-function)))
;;	   (candidates (cond (aff
;;			      (selectrum--affixate aff highlighted-candidates))
;;			     ((or annotf docsigf)
;;			      (selectrum--annotate
;;			       highlighted-candidates annotf docsigf))
;;			     (t highlighted-candidates)))
;;	   (last-title nil)
;;	   (lines nil))
;;       (dolist (cand candidates)
;;	(when-let (new-title (and groupf (funcall groupf cand nil)))
;;	  (unless (equal last-title new-title)
;;	    (push (format selectrum-group-format (setq last-title new-title)) lines)
;;	    (push "\n" lines))
;;	  (setq cand (funcall groupf cand 'transform)))
;;	(let* ((formatting-current-candidate
;;		(eq i index))
;;	       (newline
;;		(if (and formatting-current-candidate
;;			 (if (eq selectrum-extend-current-candidate-highlight
;;				 'auto)
;;			     (or aff annotf docsigf)
;;			   selectrum-extend-current-candidate-highlight))
;;		    (selectrum--selection-highlight "\n")
;;		  "\n"))
;;	       (padding
;;		(if (and formatting-current-candidate
;;			 (if (eq selectrum-extend-current-candidate-highlight
;;				 'auto)
;;			     (or aff annotf docsigf)
;;			   selectrum-extend-current-candidate-highlight))
;;		    (selectrum--selection-highlight ">  ")
;;		  "   "))
;;	       (full-cand (selectrum--format-candidate
;;			   input cand i index
;;			   first-index-displayed
;;			   'should-annotate)))
;;	  (push newline lines)
;;	  (push full-cand lines)
;;	  (push padding lines)
;;	  (cl-incf i)))
;;       (list
;;        (length highlighted-candidates)
;;        first-index-displayed
;;        (if highlighted-candidates
;;	   (apply #'concat
;;		  (append (make-list fill-rows "\n") lines))
;;	 ""))))
;;   (setq-default selectrum-display-action
;;		'(display-buffer-in-side-window
;;		  (side . bottom)
;;		  (slot . -1)
;;		  (window-parameters (mode-line-format . none))
;;		  )))

;; (use-package selectrum-prescient
;;   :ensure)

(use-package vertico
  :ensure
  :bind (:map vertico-map
	      ("C-n" . vertico-previous)
	      ("C-p" . vertico-next)
	      ("C-w" . vertico--backward-updir)
	      ([up] . vertico-next)
	      ([down] . vertico-previous))

  :init
  (defun vertico--display-in-buffer (candidates)
    (let ((buffer (get-buffer-create (format " *vertico-%s*" (- (recursion-depth) 1))))
	  (fill-rows (max 0 (- vertico-count (length candidates)))))
      (with-current-buffer buffer
	(setq-local display-line-numbers nil
		    show-trailing-whitespace nil
		    inhibit-modification-hooks t)
	(erase-buffer)
	(insert (apply #'concat (make-list fill-rows "\n")))
	(insert (string-join (nreverse candidates))) ;; NOTE: Reverse the candidates!
	(goto-char (point-min)))
      (display-buffer buffer
		      `(display-buffer-in-side-window
			(window-parameters (mode-line-format . none))
			(window-height . ,vertico-count)
			(side . bottom)
			(slot . -1)))))

  (advice-add #'vertico--display-candidates
	      :override #'vertico--display-in-buffer)

  (defun vertico--prefix-candidate (orig cand prefix suffix index)
    (setq cand (funcall orig cand prefix suffix index))
    (concat
     (if (= vertico--index index)
	 (propertize "» " 'face 'vertico-current)
       "  ")
     cand))
  (advice-add #'vertico--format-candidate :around #'vertico--prefix-candidate)

  (defun vertico--backward-updir ()
    "Delete char before or go up directory, like `ido-mode'."
    (interactive)
    (if (eq (char-before) ?/)
	(save-excursion
	  (goto-char (1- (point)))
	  (when (search-backward "/" (point-min) t)
	    (delete-region (1+ (point)) (point-max))))
      (call-interactively 'backward-kill-word)))

  (defun vertico--display-above-prompt (lines)
    (move-overlay vertico--candidates-ov (point-min) (point-min))
    (overlay-put vertico--candidates-ov 'before-string
		 (concat (make-string (- vertico-count (length lines)) ?\n)
			 (apply #'concat (nreverse lines))))
    (vertico--resize-window (length lines)))

  (advice-add #'vertico--display-candidates
	      :override #'vertico--display-above-prompt)
  )

(use-package orderless
  :ensure
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :ensure)

(use-package rg
  :ensure)

(provide 'init)
;;; init.el ends here
