;;; init.el --- Milkmacs configuration file

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("quasi-melpa" . "http://quasi.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("melpa-local" . "/Users/dcurtis/src/melpa/packages/") t)
(package-initialize)

;; ignore wiki packages
;; (defadvice package--add-to-archive-contents
;;   (around package-filter-wiki-packages (package archive) activate compile)
;;   (unless (string-match-p "\\[wiki\\]$" (package-desc-doc (cdr package)))
;;     ad-do-it))


(defvar mp-rad-packages
  '(
    ;; ace-jump-mode
    ;; ag
    ;; auto-complete
    ;; base16-theme
    ;; browse-kill-ring
    ;; clojure-mode
    ;; company
    ;; deft
    ;; diminish
    ;; dired+
    ;; evil
    ;; expand-region
    ;; flx
    ;; company
    ;; git-commit-mode
    ;; gist
    ;; ido-ubiquitous
    ;; ido-vertical-mode
    ;; magit
    ;; multiple-cursors
    ;; rainbow-delimiters
    ;; smartparens
    ;; smex
    ;; undo-tree
    ;; dropdown-list
    ))

(defun mp-install-rad-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        mp-rad-packages))


(defun mp-build-rad-packages ()
  (interactive)
  (mapc #'(lambda (package)
            (package-build-archive package))
        mp-rad-packages))


;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))


;; (add-to-list 'load-path "~/src/powerline/")
;; (require 'powerline)
;; (setq powerline-default-separator 'butt)
;; (powerline-reset)
;; (powerline-default-theme)

;;;; external libraries
(require 'checkdoc)
(require 'midnight)
(require 'misc)
(require 'recentf)
(require 'saveplace)
(require 'uniquify)


(setq mp-extra-paths
      '("~/.cabal/bin/"
        "/usr/local/share/npm/bin/"
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

;;;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'eshell/ff 'find-file)
(defalias 'eshell/ffow 'find-file-other-window)
(defalias 'yes-or-no-p 'y-or-n-p)


;;;; random number generator
(random t)

(auto-insert-mode t)

(windmove-default-keybindings)
(global-set-key (kbd "<select>") 'windmove-up)
;; (global-set-key (kbd "s-l") 'windmove-right)
;; (global-set-key (kbd "s-h") 'windmove-left)
;; (global-set-key (kbd "s-j") 'windmove-down)
;; (global-set-key (kbd "s-k") 'windmove-up)


;;;; remaps
(define-key key-translation-map (kbd "<C-tab>") (kbd "M-TAB"))
(define-key key-translation-map (kbd "C-x C-m") (kbd "M-x"))
(define-key key-translation-map (kbd "C-x C-d") (kbd "C-x d"))


;;;; global key bindings
(global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-M-SPC") 'just-one-space)
;;(global-set-key (kbd "A-h") 'ns-do-hide-emacs)
;;(global-set-key (kbd "A-M-h") 'ns-do-hide-others)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "C-z") 'other-window)
(global-set-key (kbd "C-S-z") 'other-window-reverse)



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
(global-set-key (kbd "C-\\") 'ctl-x-4-prefix)

(global-set-key (kbd "M-RET") 'open-next-line)
(global-set-key (kbd "C-o") 'open-line-indent)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-M-<return>") 'new-line-in-between)

(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c r") 'iterm-run-previous-command)

(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") 'copy-whole-lines)

;; Eval buffer
(global-set-key (kbd "C-c v") 'eval-buffer)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

(global-set-key (kbd "C-h") (kbd "<DEL>"))

;;(global-set-key (kbd "C-c h") 'help-command)

(global-set-key (kbd "C-S-y") 'yank-unindented)

(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)

(global-set-key (kbd "C-c w") 'copy-paste)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-x C-i") 'imenu)

(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-M-/") 'hippie-expand-line)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-M-q") 'unfill-paragraph)

(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

(global-set-key (kbd "C-'") 'toggle-quotes)

(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)

(global-set-key (kbd "C-+") 'change-number-at-point)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-and-file)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-c h") 'help-command)

(define-key 'help-command "a" 'apropos)


;;;; advice
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;;;; hooks
(add-hook 'write-file-functions 'time-stamp)


;;;; generic
(blink-cursor-mode nil)
(column-number-mode t)
                                        ;(desktop-save-mode t)
(global-auto-revert-mode t)
(recentf-mode t)
(savehist-mode t)
(show-paren-mode t)
(visual-line-mode -1)
(winner-mode t)
(global-subword-mode t)
(delete-selection-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(which-function-mode t)


;;;; the uncustomizable
(setq-default
 ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]
 ansi-color-for-comint-mode t
 ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
 auto-revert-verbose nil
 auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosave/" t)))
 backup-directory-alist (quote (("." . "~/.emacs.d/backups/")))
 backward-delete-char-untabify-method nil
 coffee-tab-width 2
 cua-enable-cua-keys nil
 custom-theme-directory "~/.emacs.d/themes/"
 delete-auto-save-files nil
 diff-switches "-u"
 dired-use-ls-dired nil
 echo-keystrokes 0.1
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 enable-recursive-minibuffers t
 erc-hide-list '("JOIN" "PART" "QUIT")
 flymake-gui-warnings-enabled t
 fringe-indicator-alist '((truncation left-arrow right-arrow)
                          (continuation nil right-curly-arrow)
                          (overlay-arrow . right-triangle)
                          (up . up-arrow)
                          (down . down-arrow)
                          (top top-left-angle top-right-angle)
                          (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                          (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                          (empty-line . empty-line)
                          (unknown . question-mark))
 global-auto-revert-non-file-buffers t
 ibuffer-expert t
 ibuffer-show-empty-filter-groups nil
 imenu-auto-rescan t
 indent-tabs-mode nil
 indicate-empty-lines t
 ispell-extra-args (quote ("--sug-mode=ultra"))
 ispell-program-name "aspell"
 line-spacing 0
 locale-coding-system 'utf-8
 mode-line-in-non-selected-windows t
 mode-line-inverse-video t
 mouse-wheel-scroll-amount (quote (0.01))
 mouse-yank-at-point t
 ns-alternate-modifier (quote super)
 ns-command-modifier (quote meta)
 ns-function-modifier (quote hyper)
 ns-pop-up-frames nil
 ns-tool-bar-display-mode 'both
 ns-tool-bar-size-mode 'regular
 recentf-max-saved-items 100
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
 show-paren-style 'mixed
 split-height-threshold nil
 split-width-threshold 159
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)"
 tramp-remote-path '(tramp-default-remote-path tramp-own-remote-path "/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")
 undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/")))
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re "^\\*"
 uniquify-separator " • "
 user-full-name "Donald Ephraim Curtis"
 user-mail-address "dcurtis@milkbox.net"
 visible-bell nil
 whitespace-style '(face tabs trailing lines-tail newline indentation empty space-after-tab)
 whitespace-style '(face tabs trailing lines-tail newline empty space-after-tab))


;;;; Darwin specific
(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")
       (setenv
        "PYTHONPATH"
        "/Users/dcurtis/src/compepi:/Users/dcurtis/src/networkx")))

;;;; GUI settings
(when (display-graphic-p)
  (fringe-mode 1)
  (mouse-wheel-mode t)
  (menu-bar-mode t)
  (setq-default mac-option-modifier 'super)
  (setq-default mac-pass-command-to-system nil)

  (set-face-attribute 'default nil :font "DejaVu Sans-12")
  ;; (set-face-attribute 'default nil :font "Inconsolata-13")
  ;; specify a unicode font : MENLO (forced normal)
  (set-fontset-font "fontset-default" 'unicode "-apple-Inconsolata-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")

  ;; for the height, subtract a couple hundred pixels
  ;; from the screen height (for panels, menubars and
  ;; whatnot), then divide by the height of a char to
  ;; get the height we want
  ;; (set-frame-size (selected-frame) 0 0)
  ;; (add-to-list 'initial-frame-alist '(top . 10))
  ;; (add-to-list 'initial-frame-alist '(left . 30))
  ;; (set-frame-size (selected-frame))
  ;; (set-frame-size (selected-frame) )
  (add-to-list 'initial-frame-alist '(left . 1))
  (add-to-list 'initial-frame-alist '(top . 1))

  (cond
   ((string-equal system-name "hendrix.local")
    (add-to-list 'initial-frame-alist
                 (cons 'width
                       (/ (ceiling (* (- (display-pixel-width)
                                         (apply '+ (cl-remove-if (lambda (i) (not i))
                                                                 (window-fringes))))
                                      .4))
                          (frame-char-width))))
    (add-to-list 'initial-frame-alist (cons 'height (/ (display-pixel-height)
                                                       (frame-char-height)))))
   (t (add-to-list 'initial-frame-alist
                   (cons 'width
                         (/ (ceiling (* (- (display-pixel-width)
                                           (apply '+ (cl-remove-if (lambda (i) (not i))
                                                                   (window-fringes))))
                                        .667))
                            (frame-char-width))))
      (add-to-list 'initial-frame-alist (cons 'height (/ (display-pixel-height)
                                                         (frame-char-height)))))))

;;;; mode-line
(setq mode-line-modes
      (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
        (list (propertize "%[" 'help-echo recursive-edit-help-echo)
              "("
              `(:propertize ("" mode-name)
                            face font-lock-string-face
                            help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                            mouse-face mode-line-highlight
                            local-map ,mode-line-major-mode-keymap)
              '("" mode-line-process)
              `(:propertize ("" minor-mode-alist)
                            mouse-face mode-line-highlight
                            help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                            local-map ,mode-line-minor-mode-keymap)
              (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                          'mouse-face 'mode-line-highlight
                          'local-map (make-mode-line-mouse-map
                                      'mouse-2 #'mode-line-widen))
              ")"
              (propertize "%]" 'help-echo recursive-edit-help-echo)
              " ")))

(setq mode-line-position
      `((-3 ,(propertize
              "%p"
              'face 'font-lock-constant-face
              'local-map mode-line-column-line-number-mode-map
              'mouse-face 'mode-line-highlight
              ;; XXX needs better description
              'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu"))
        (size-indication-mode
         (8 ,(concat " / "
                     (propertize
                      "%I"
                      'face 'font-lock-constant-face
                      'local-map mode-line-column-line-number-mode-map
                      'mouse-face 'mode-line-highlight
                      ;; XXX needs better description
                      'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu"))))
        (line-number-mode
         ((column-number-mode
           (10 ,(concat " ("
                        (propertize
                         "%l"
                         'face 'font-lock-type-face
                         'local-map mode-line-column-line-number-mode-map
                         'mouse-face 'mode-line-highlight
                         'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu")
                        ","
                        (propertize
                         "%c"
                         'face 'font-lock-type-face
                         'local-map mode-line-column-line-number-mode-map
                         'mouse-face 'mode-line-highlight
                         'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu")
                        ")"))
           (6 ,(propertize
                " L%l"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu"))))
         ((column-number-mode
           (5 ,(propertize
                " C%c"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu")))))))


;;;; faces
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


;;;; hippie-expand
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))



;;;; dired
(global-set-key (kbd "C-x C-j") 'dired-jump)
(define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)
(global-set-key (kbd "M-s f") 'find-name-dired)

(after 'dired
  (define-key dired-mode-map (kbd "M-p") 'dired-back-to-top)
  (define-key dired-mode-map (kbd "M-n") 'dired-jump-to-bottom)
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files))


;;;; diminish
(after "diminish-autoloads"
  (after 'paredit (diminish 'paredit-mode " pe"))
  (after 'yasnippet (diminish 'yas-minor-mode " ys"))
  (after 'undo-tree (diminish 'undo-tree-mode " ut"))
  (after 'checkdoc (diminish 'checkdoc-minor-mode " cd"))
  (after 'company (diminish 'company-mode " c")))


;;;; smartparens
(after "smartparens-autoloads"
  (require 'smartparens-config)
  (smartparens-global-mode t))

(after 'smartparens
  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-autoinsert-quote-if-followed-by-closing-pair t)
  (setq sp-autoinsert-if-followed-by-word t)
  (setq sp-wrap-entire-symbol nil)
  (sp-pair "\"" nil)
  (sp-with-modes sp--lisp-modes (sp-local-pair "(" nil :bind "M-("))
  ;; (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  ;; (define-key sp-keymap (kbd ")") 'sp-up-sexp)
  ;; (define-key sp-keymap (kbd "]") 'sp-up-sexp)
  ;; (define-key sp-keymap (kbd "}") 'sp-up-sexp)
  (show-smartparens-global-mode t))


;;;; phi-search
(after "phi-search-autoloads"
  (after 'multiple-cursors
    (define-key mc/keymap (kbd "C-s") 'phi-search)
    (define-key mc/keymap (kbd "C-p") 'phi-search-backward)))


;;;; surround-mode
(global-set-key (kbd "M-C") 'surround-change)

;;;; smartrep
(after "smartrep-autoloads"
  (require 'smartrep))

;;;; term-mode
(setq system-uses-terminfo nil)

;;;; helm
(after 'helm
  (setq helm-ff-auto-update-initial-value nil)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-h") (kbd "<DEL>"))
  (define-key helm-map (kbd "C-w") 'subword-backward-kill)
  (define-key helm-map (kbd "M-w") 'helm-yank-text-at-point)
  (setq helm-quick-update t)
  (setq helm-follow-mode-persistent t))

(after 'helm-git-grep
  (define-key helm-git-grep-map (kbd "C-w") 'subword-backward-kill))


;;;; ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-prospects 10)
(setq ido-read-file-name-non-ido nil)
(setq ido-use-filename-at-point nil)
(setq ido-use-virtual-buffers t)

(global-set-key (kbd "C-x f") 'find-file-in-project)
(define-key ctl-x-4-map (kbd "f") 'find-file-in-project-other-window)
(define-key ctl-x-4-map (kbd "s") 'shell-other-window)

(defun mp-ido-hook ()
  (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
  (define-key ido-completion-map [tab] 'ido-complete))

(add-hook 'ido-setup-hook 'mp-ido-hook)


;;;; ido-ubiquitous
(after "ido-ubiquitous-autoloads" (ido-ubiquitous-mode t))
;;(after 'ido-ubiquitous (ido-ubiquitous-disable-in evil-ex))

(setq ido-ubiquitous-command-exceptions '(evil-ex execute-extended-command))
(setq ido-ubiquitous-function-exceptions '(grep-read-files ucs-insert))


;;;; ido-vertical-mode
(after "ido-vertical-mode-autoloads"
  (ido-vertical-mode t))


;;;; flx
(after "flx-ido-autoloads"
  (setq ido-use-faces nil)
  (flx-ido-mode t))

;;;; ido-at-point
(after "ido-at-point-autoloads"
  (ido-at-point-mode))

;;;; smex
(after "smex-autoloads" (smex-initialize))

(after 'smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;;;; ispell
(setq ispell-list-command "list")

;;;; diff commands
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;;;; yas/snippets
(after 'yasnippet
  (yas/reload-all)
  (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt yas/no-prompt)))

(after "yasnippet-autoloads"
  (add-hook 'prog-mode-hook 'yas-minor-mode))


;;;; js2-mode
(after "js2-mode-autoloads"
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;;; jinja2-mode
(after "jinja2-mode-autoloads"
  (add-to-list 'auto-mode-alist '("\\.html$" . jinja2-mode)))

(after 'jinja2-mode
  (add-to-list 'jinja2-user-keywords "assets"))

;;;; expand-region
(after "expand-region-autoloads"
  (global-set-key (kbd "C-c w") 'er/expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))


;;;; anzu
(after "anzu-autoloads"
  (global-anzu-mode))


;;;; jump-char
(after "jump-char-autoloads"
  (global-set-key (kbd "M-m") 'jump-char-forward)
  (global-set-key (kbd "M-M") 'jump-char-backward))

(after 'jump-char
  (setq jump-char-lazy-highlight-face nil))


;;;; ace-jump-mode
(after "ace-jump-mode-autoloads"
  (define-key global-map (kbd "C-;") 'ace-jump-mode))

;;;; wrap-region
(after "wrap-region-autoloads"
  (setq wrap-region-only-with-negative-prefix t)
  (wrap-region-global-mode t))


;;;; multiple-cursors
(after "multiple-cursors-autoloads"
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c <return>") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-<return>") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-=") 'mc/insert-numbers)
  (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c *") 'mc/mark-all-like-this)
  (global-set-key (kbd "s-SPC") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (defadvice require (around require-advice (feature &optional filename noerror) activate)
    (save-excursion
      (let (deactivate-mark)
        ad-do-it))))



;;;; rainbow-delimiters
(after "rainbow-delimiters-autoloads"
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))


;;;; change-inner
;; (after "change-inner-autoloads"
;;   (global-set-key (kbd "M-I") 'change-inner)
;;   (global-set-key (kbd "M-O") 'change-outer))

;;;; undo-tree
(after "undo-tree-autoloads"
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))



;;;; evil-mode
(after 'evil
  (define-key evil-normal-state-map (kbd "C-y") 'evil-paste-before)
  ;; (define-key evil-ex-map "e " 'ido-find-file)
  ;; (define-key evil-ex-map "w " 'ido-write-file)
  ;; (define-key evil-ex-map "b " 'ido-switch-buffer)
  (when (boundp 'global-surround-mode) (global-surround-mode))
  (ignore-errors (require 'evil-leader)))

;;;; evil-leader
(after 'evil-leader
  (evil-leader/set-key (kbd "t") 'find-file-in-project))


;;;; hl-sentence
(after "hl-sentence-autoloads"
  (add-hook 'LaTeX-mode-hook 'hl-sentence-mode))


;;;; auctex
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


;;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(after 'magit
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))


;;;; deft
(after 'deft
  (setq deft-directory (expand-file-name "~/Dropbox/notes"))
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t))


;;;; markdown
(setq auto-mode-alist
      (cons '("\\.te?xt\\'" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm?d\\'" . markdown-mode) auto-mode-alist))
(setq markdown-command "pandoc -S")

(defun markdown-latex-narrow ()
  (interactive)
  (setq markdown-latex-command
        "pandoc --template=$HOME/Dropbox/Resources/latex/pandocnarrow.tex -s -t latex -Vfontsize:10pt")
  (setq markdown-pandoc-pdf-command
        "pandoc --template=$HOME/Dropbox/Resources/latex/pandocnarrow.tex -s -Vfontsize:10pt"))

(defun markdown-latex-wide ()
  (interactive)
  (setq markdown-latex-command
        "pandoc --template=$HOME/Dropbox/Resources/latex/pandocwide.tex -s -t latex -Vfontsize:10pt")
  (setq markdown-pandoc-pdf-command
        "pandoc --template=$HOME/Dropbox/Resources/latex/pandocwide.tex -s -Vfontsize:10pt"))

(defun markdown-latex-medium ()
  (interactive)
  (setq markdown-latex-command
        "pandoc --template=$HOME/Dropbox/Resources/latex/pandocmedium.tex -s -t latex -Vfontsize:10pt")
  (setq markdown-pandoc-pdf-command
        "pandoc --template=$HOME/Dropbox/Resources/latex/pandocmedium.tex -s -Vfontsize:11pt"))

(after 'markdown-mode
  (remove-hook 'text-mode-hook 'turn-on-auto-fill)
  (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab)
  (define-key markdown-mode-map (kbd "C-c r") 'markdown-copy-rtf)
  (define-key markdown-mode-map (kbd "C-c l") 'markdown-export-latex)
  (define-key markdown-mode-map (kbd "C-c v") 'marked)
  (define-key markdown-mode-map (kbd "C-c w") 'markdown-select-section-copy-paste)
  (define-key markdown-mode-map (kbd "C-c s") 'markdown-select-section)
  (define-key markdown-mode-map (kbd "C-c c") 'markdown-copy-html)
  (define-key markdown-mode-map (kbd "C-c p") 'markdown-export-pdf)
  (define-key markdown-mode-map (kbd "C-c q") 'markdown-copy-paste-html)
  (define-key markdown-mode-map (kbd "C-c =") 'markdown-cleanup-list-numbers))

(add-hook 'markdown-mode-hook 'abbrev-mode)
(add-hook 'markdown-mode-hook 'toggle-word-wrap)


;;;; prog-mode
(defun mp-buffer-enable-whitespace-cleanup ()
  "enable whitespace-cleanup in the current buffer"
  (interactive)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(defun mp-buffer-disable-whitespace-cleanup ()
  "enable whitespace-cleanup in the current buffer"
  (interactive)
  (remove-hook 'before-save-hook 'whitespace-cleanup t))

(add-hook 'prog-mode-hook 'mp-buffer-enable-whitespace-cleanup)
(add-hook 'prog-mode-hook 'whitespace-mode)
;; (add-hook 'prog-mode-hook 'hl-line-mode)
;; (add-hook 'prog-mode-hook 'toggle-truncate-lines)


;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '(nil "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; hl-sexp
(after "hl-sexp-autoloads"
  (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
(add-font-lock-numbers 'emacs-lisp-mode)

(defun mp-buffer-enable-reindent ()
  "Enable `indent-buffer' on the current buffer."
  (interactive)
  (add-hook 'before-save-hook 'indent-buffer nil t))

(defun mp-buffer-disable-reindent ()
  "Enable `indent-buffer' on the current buffer."
  (interactive)
  (remove-hook 'before-save-hook 'indent-buffer t))

(add-hook 'emacs-lisp-mode-hook 'mp-buffer-enable-reindent)
(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)


;;;; clojure
(add-hook 'clojure-mode-hook 'mp-buffer-enable-reindent)


;;;; paredit
(after "paredit-autoloads"

  ;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))

  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))


;;;; c / c++ mode
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

(after 'emacs-major-version)


;;;; company
(after 'company
  (add-to-list 'company-backends 'company-capf)
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (setq company-idle-delay 0.1)
  (setq company-begin-commands '(self-insert-command))
  (define-key company-active-map (kbd "C-w") nil))


;;;; auto-complete
(after 'auto-complete
  (setq ac-auto-show-menu .1)
  (setq ac-use-menu-map t)
  (setq ac-disable-inline t)
  (setq ac-candidate-menu-min 0)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict"))

(after 'auto-complete-config
  ;; (ac-config-default)
  (add-hook 'ein:notebook-multilang-mode-hook 'auto-complete-mode)
  (setq-default ac-sources (append '(ac-source-yasnippet ac-source-imenu) ac-sources))
  (when (file-exists-p (expand-file-name "~/.emacs.d/elisp/Pymacs"))
    (ac-ropemacs-initialize)
    (ac-ropemacs-setup)))

(after "auto-complete-autoloads"
  (require 'auto-complete-config))

(when (file-exists-p (expand-file-name "~/.emacs.d/elisp/Pymacs"))
  (setq ropemacs-enable-autoimport t)
  (add-to-list 'load-path "~/.emacs.d/elisp/Pymacs"))


;;;; python
(add-font-lock-numbers 'python-mode)

(defun python-config-python ()
  "Configure python.el to defaults of using python."
  (interactive)
  (setq python-shell-virtualenv-path "venv"
        python-shell-interpreter "python"
        python-shell-prompt-regexp ">>> "
        python-shell-prompt-output-regexp ""
        ;; python-shell-setup-codes '(python-shell-completion-setup-code python-ffap-setup-code python-eldoc-setup-code)
        python-shell-completion-module-string-code ""
        python-shell-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))
"))

(defun python-config-ipython ()
  "Configure python.el to handle ipython."
  (interactive)
  ;; (add-to-list 'python-shell-setup-codes 'python-shell-icompletion-setup-code)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        ;; python-shell-setup-codes '(python-shell-icompletion-setup-code python-ffap-setup-code python-eldoc-setup-code)
        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(setq python-shell-icompletion-setup-code "from IPython.core.completerlib import module_completion")
(setq python-shell-virtualenv-path "~/.virtualenv/default")

(after 'python (python-config-ipython))

(setq ein:use-smartrep nil
      ein:use-auto-complete t
      ein:complete-on-dot t
      ein:notebook-console-executable (expand-file-name "~/.virtualenv/default/bin/ipython")
      ein:notebook-console-security-dir (expand-file-name "~/.ipython/profile_default/security"))


(defun python-modes-init ()
  "initialization for all python modes"
  (interactive)
  (make-face 'font-lock-statement-face)
  (set-face-attribute
   'font-lock-statement-face nil :inherit font-lock-variable-name-face)
  (setq font-lock-statement-face 'font-lock-statement-face)
  (font-lock-add-keywords
   'python-mode
   `((,(rx symbol-start (or "def" "class" "return" "as" "try" "except" "raise") symbol-end)
      0 font-lock-statement-face)))


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

  (font-lock-add-keywords
   'python-mode
   `(("^[       ]*\\(@\\)\\([a-zA-Z_][a-zA-Z_0-9.]+\\)\\((.+)\\)?"
      (1 'font-lock-keyword-face)
      (2 'font-lock-function-name-face))))

  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error))

;;(after 'python (python-modes-init))

;;;; pyflakes
(defun mp-flymake-pyflakes-init (&optional trigger-type)
  ;; Make sure it's not a remote buffer or flymake would not work
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-with-folder-structure))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (expand-file-name "~/.virtualenv/bin/pyflakes") (list temp-file))))

(after 'flymake
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" mp-flymake-pyflakes-init)))



;;;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;;;; android-mode
(after "android-mode-autoloads"
  (setq android-mode-sdk-dir "~/opt/android"))


;;;; ruby
(after 'ruby-mode
  (add-hook 'ruby-mode-hook 'run-prog-mode-hook))

(after "rvm-autoloads"
  (add-hook 'ruby-mode-hook 'rvm-use-default))

(after 'find-file-in-project
  (add-to-list 'ffip-patterns "*.c")
  (add-to-list 'ffip-patterns "*.java")
  (add-to-list 'ffip-patterns "*.xml")
  (add-to-list 'ffip-patterns "*.jsp")
  (add-to-list 'ffip-patterns "*.less")
  (add-to-list 'ffip-patterns "*.coffee")
  (add-to-list 'ffip-patterns "*.css")
  (add-to-list 'ffip-patterns "*.h"))


;;;; html
(add-font-lock-numbers 'html-mode)


;;;; php
(setq auto-mode-alist
      (cons '("\\.php[345]?\\'\\|\\.phtml\\." . php-mode) auto-mode-alist))


;;;; mmm-mode
(after "mmm-mode-autoloads"
  (require 'mmm-auto)
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
  (mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'embedded-css)
  ;; (setq mmm-submode-decoration-level 2)
  ;; (setq nxml-slash-auto-complete-flag t)
  ;; (mmm-add-mode-ext-class 'nxml-mode "\\.php\\'" 'html-php)
  ;; (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
  )


;;;; nxhtml
(after "nxhtml-autoloads"
  (autoload 'django-html-mumamo-mode
    (expand-file-name "autostart.el"
                      (file-name-directory (locate-library "nxhtml-autoloads"))))
  (setq auto-mode-alist
        (append '(("\\.html?$" . django-html-mumamo-mode)) auto-mode-alist))
  (setq mumamo-background-colors nil)
  (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode)))


;;;; pony-mode
(after 'pony-mode
  (setq pony-snippet-dir
        (expand-file-name
         "snippets/"
         (file-name-directory (locate-library "pony-mode")))))


;;;; octave-mode
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(setenv "GNUTERM" "x11")


;;;; defun
(defvar yank-indent-modes '(prog-mode
                            js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defun yank-unindented ()
  (interactive)
  (yank 1))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun delete-current-buffer-file ()
  "Deletes current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (delete-file filename)
      (kill-this-buffer))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
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


(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(defun paste-previous-osx-app ()
  "paste the current buffer into the previous OS X application.
either the one specified in the '.meta' file or the previously
used app."
  (interactive)
  (do-applescript
   (concat
    (let ((metafn (concat (buffer-file-name) ".meta")))
      (cond
       ((and (buffer-file-name) (file-exists-p metafn))
        (save-buffer)
        (with-temp-buffer
          (insert-file-contents-literally metafn)
          (goto-char (point-min))
          (do-applescript
           (concat
            "tell application \""
            (buffer-substring-no-properties (point-at-bol) (point-at-eol))
            "\" to activate"))))
       (t
        "
tell application \"System Events\" to keystroke tab using {command down}
delay 0.2"
        )))
    "
tell application \"System Events\" to keystroke \"a\" using {command down}
tell application \"System Events\" to keystroke \"v\" using {command down}")))


(defun markdown-select-section-copy-paste (level)
  "Select the LEVEL or current, when nil, section of markdown and copy and paste it."
  (interactive "P")
  (markdown-select-section level)
  (copy-paste))

(defun copy-paste ()
  "Copy the buffer and paste it into the previous buffer or that determined by the '.meta' file."
  (interactive)
  (save-excursion
    (let ((begin-region)
          (end-region))
      (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
          (setq begin-region (region-beginning)
                end-region (region-end))
        (setq begin-region (point-min)
              end-region (point-max)))
      (kill-ring-save begin-region end-region))
    (paste-previous-osx-app)))



(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we
   are not at the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts
   comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))


(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (i num)
      (insert region))))


(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))



(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory))))

(defun marked ()
  "Open the current file in Marked."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)
    (shell-command (concat "open -a Marked "
                           (shell-quote-argument buffer-file-name)))))

(defun make-executable ()
  "Make the current file loaded in the buffer executable"
  (interactive)
  (if (buffer-file-name)
      (start-file-process "Make Executable" nil "/bin/bash" "-c"
                          (concat "chmod u+x " (file-name-nondirectory buffer-file-name)))
    (message "Buffer has no filename.")))


(defun width-80 ()
  (interactive)
  (set-window-margins (selected-window) 0 0)
  (let ((marginwidth (/ (- (window-width) 80) 2)))
    (set-window-margins (selected-window) marginwidth marginwidth)))


(defun setup-local-iterm ()
  "locally define C-c C-c to run the iterm-run-previous-command"
  (interactive)
  (local-set-key (kbd "C-c C-c") 'iterm-run-previous-command))


(defun iterm-run-previous-command ()
  "applescript to switch to iTerm and run the previously run command"
  (interactive)
  (save-buffer)
  (do-applescript "
tell application \"Terminal\"
activate
tell application \"System Events\"
keystroke \"p\" using {control down}
keystroke return
end tell
end tell"))


(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))



(defun mpround ()
  "round the current floating-point"
  (interactive)
  (save-excursion
    (let* ((start (point)) (end (point)))
      (forward-word 2)
      (setq end (point))
      (insert
       (number-to-string
        (/ (round
            (* (string-to-number
                (buffer-substring-no-properties start end)) 1000.0))  1000.0)))
      (delete-region start end))))



(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (when (eolp)
    (save-excursion
      (delete-region (point)
                     (progn (skip-chars-backward " \t") (point)))))
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))


(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (newline-and-indent))


(defun open-line-indent (n)
  "Insert a new line and leave point before it. With arg N insert N newlines."
  (interactive "*p")
  (save-excursion
    (newline n)
    (indent-according-to-mode)))


(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))


(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))


(defun TeX-compile ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LaTeX" 'TeX-active-master 0))


(defun compile-make ()
  (interactive)
  (save-buffer)
  (compile "make -k"))


(defun font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))

(defun c-snug-if (syntax pos)
  "Dynamically calculate brace hanginess for do-while statements.
Using this function, `while' clauses that end a `do-while' block will
remain on the same line as the brace that closes that block.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `block-close' syntax."
  (save-excursion
    (let (langelem)
      (if (and (eq syntax 'substatement-open)
               (setq langelem (assq 'substatement-open c-syntactic-context))
               (progn (goto-char (c-langelem-pos langelem))
                      (if (eq (char-after) ?{)
                          (c-safe (c-forward-sexp -1)))
                      (looking-at "\\<if\\>[^_]")))
          '(after)
        '(before after)))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))


(defun orgtbl-to-pandoc-cell (val colwidth align)
  "Convert an `org-mode' table cell to pandoc.

Format VAL for COLWIDTH column and specified ALIGN."
  (setq colwidth (+ 2 colwidth))
  (if align
      (concat (make-string (- colwidth (length val)) ? ) val)
    (concat val (make-string (- colwidth (length val)) ? ))))


(defun orgtbl-to-pandoc (table params)
  "Convert `orgtbl' TABLE from to a pandoc table with given PARAMS."
  (let* ((splicep (plist-get params :splice))
         (html-table-tag org-export-html-table-tag)
         html)
    ;; Just call the formatter we already have
    ;; We need to make text lines for it, so put the fields back together.
    (concat "\n"
            (mapconcat
             'identity
             (mapcar
              (lambda (x)
                (if (eq x 'hline)
                    (mapconcat
                     'identity
                     (mapcar
                      (lambda (colwidth)
                        (make-string (1+ colwidth) ?-))
                      org-table-last-column-widths) " ")
                  (mapconcat
                   'identity
                   (mapcar*
                    'orgtbl-to-pandoc-cell
                    x
                    org-table-last-column-widths
                    org-table-last-alignment) " ")))
              table)
             "\n")
            "\n")))


(defvar wikipedia-url "http://en.wikipedia.org/wiki/%s" "Wikipedia URL")


(defun wikicase (str)
  "change string to wikipedia case"
  (mapconcat 'capitalize (split-string str) "_"))


(defun markdown-wikipedia-link ()
  "Insert a link to wikipedia based on the name of the current keyword link."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (re-search-forward "\\[\\(.+\\)\\]:" (point-at-eol))
    (end-of-line)
    (insert (format wikipedia-url (wikicase (match-string 1))))))

;; kill region if active, otherwise kill backward word
(defun kill-region-or-backward-word (arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (call-interactively (key-binding (kbd "M-<DEL>")) t (this-command-keys-vector))))

(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

;; copy region if active
;; otherwise copy to end of current line
;;   * with prefix, copy N whole lines

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy ARG lines to the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or ARG lines."
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))


;; M-up is nicer in dired if it moves to the third line - straight to the ".."
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 2)
  (dired-back-to-start-of-files))

;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -1)
  (dired-back-to-start-of-files))

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)))

(defun toggle-window-split ()
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
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))


(defun run-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(defun other-window-reverse ()
  "Select the other window but in reverse."
  (interactive)
  (other-window -1))

(defun shell-other-window (&optional buffer)
  (interactive
   (list
    (and current-prefix-arg
         (prog1
             (read-buffer "Shell buffer: "
                          (generate-new-buffer-name "*shell*"))
           (if (file-remote-p default-directory)
               ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
               (setq default-directory
                     (expand-file-name
                      (read-directory-name
                       "Default directory: " default-directory default-directory
                       t nil))))))))
  (let ((buffer (save-window-excursion
                  (shell buffer))))
    (switch-to-buffer-other-window buffer)))

(defun find-file-in-project-other-window ()
  "Find a file in the current project in the other window."
  (interactive)
  (let ((buffer (save-window-excursion (find-file-in-project))))
    (switch-to-buffer-other-window buffer)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun hippie-expand-line ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line
                                            try-expand-line-all-buffers)))
    (hippie-expand nil)))


;; toggle quotes
(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun alternate-quotes-char ()
  (if (eq ?' (current-quotes-char)) ?\" ?'))

(defun toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
      (let ((old-quotes (char-to-string (current-quotes-char)))
            (new-quotes (char-to-string (alternate-quotes-char)))
            (start (make-marker))
            (end (make-marker)))
        (save-excursion
          (move-point-forward-out-of-string)
          (backward-delete-char 1)
          (set-marker end (point))
          (insert new-quotes)
          (move-point-backward-out-of-string)
          (delete-char 1)
          (insert new-quotes)
          (set-marker start (point))
          (replace-string new-quotes (concat "\\" new-quotes) nil start end)
          (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))



;;; customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector [unspecified "#081724" "#ff694d" "#68f6cb" "#fffe4e" "#bad6e2" "#afc0fd" "#d2f1ff" "#d3f9ee"])
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (mustang)))
 '(custom-safe-themes (quote ("6e05b0a83b83b5efd63c74698e1ad6feaddf69a50b15a8b4a83b157aac45127c" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "f48b43277382ee56a9e3c5d08b71c3639f401f6338da00039dcac3c21c0811b5" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "7d090d4b15e530c28a35e084700aca464291264aad1fa2f2b6bba33dd2e92fd5" "b674ccba78eb688bea71ea8a1fd2782fcd69bd462e2504008903b5b6e018b480" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "af5f7d472ffeed85823d563512ce95e8ff7d648d8217d9ab79b17ae601b9a9d5" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "e57e7b19da7b4cd0e5512d5e9bc20d31c9cf50112c462de15a76bce0ea3c5ef5" "75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" "d971315c813b0269a86e7c5e73858070063016d9585492bd8d0f27704d50fee7" "450b29ed22abeeac279b7eeea592f4eea810105737716fc29807e1684e729c55" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "d7f1c86b425e148be505c689fc157d96323682c947b29ef00cf57b4e4e46e6c7" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "eacfc96fbe418c017f4a00fdde5d5029db8e2800a46251eb2174484fa431917e" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "98522c31200ec2ee2c84ae3dddac94e69730650096c3f4f751be4beece0f6781" "eef383086ae1f47f75cda814adfb9868a3dafa99b6eb67fdff780a52d326d468" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "465be5317c7d95a84e376e095c21242f4f2ad75692ed806dcbb6fe27078260f1" "f24462e7d2dcc5a7c6767ffc25cab208e6e3a963682b62bb2526eceb3899fff8" "f0283ec7a2385af8ea0fbb1466f340bbd0b2cededa60831394ec94e98096e1a8" "1d10043a0318d90a71cfa1f7e6b3b67f77c16ff9854f35a5b8722d87cb74f580" "b8f561a188a77e450ab8a060128244c81dea206f15c1152a6899423dd607b327" "1c1e6b2640daffcd23b1f7dd5385ca8484a060aec901b677d0ec0cf2927f7cde" "1a093e45e4c3e86fa5ad1f8003660e7cda4d961cd5d377cee3fee2dad2faf19b" "78cfbd96775588c06c4fff22573aaa5fa762ca2b8eda43cb964b7739194ed3c1" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "76b9b3780c4844712e4a3ab05b8669eecd56a3864aae29e54005ffc68c24414c" "bb1eaf74fcfa24c8868078cac73abba4138d0ddb7f11f44d7e8f849edbf8b912" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "466ae54a7b157ad02fd91da72b7871bccfb9bac98fdab95cf7a0d405c8572bd0" "88b663861db4767f7881e5ecff9bb46d65161a20e40585c8128e8bed8747dae5" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "9562e9eb5fd01677ac6b76b66f9a81338991fa9d270270802eeae5232c8da0a6" "6f3060ac8300275c990116794e1ba897b6a8af97c51a0cb226a98759752cddcf" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" default)))
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#444444")
 '(foreground-color "#cccccc")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode (quote (4 . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote left))
 '(linum-format " %7i ")
 '(overflow-newline-into-fringe t)
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode) (when (fboundp (quote flycheck-mode)) (flycheck-mode -1)) (unless (featurep (quote package-build)) (let ((load-path (cons ".." load-path))) (require (quote package-build)))) (package-build-minor-mode)) (eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode) (unless (featurep (quote package-build)) (let ((load-path (cons ".." load-path))) (require (quote package-build)))) (package-build-minor-mode)))))
 '(vc-annotate-background monokai-bg)
 '(vc-annotate-color-map (quote ((20 . monokai-fg-1) (40 . monokai-bg+2) (60 . monokai-red) (80 . monokai-red+1) (100 . monokai-orange) (120 . monokai-orange+1) (140 . monokai-green) (160 . monokai-green+1) (180 . monokai-yellow) (200 . monokai-yellow+1) (220 . monokai-blue) (240 . monokai-blue+1) (260 . monokai-purple) (280 . monokai-purple+1) (300 . monokai-cyan) (320 . monokai-cyan+1) (340 . monokai-magenta) (360 . monokai-magenta+1))))
 '(vc-annotate-very-old-color monokai-magenta)
 '(virtualenv-root "/Users/dcurtis/.virtualenv/"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-sentence-face ((t (:foreground "white"))) t)
 '(variable-pitch ((t (:height 120 :family "DejaVu Sans")))))


(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

;;; init.el ends here

;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
