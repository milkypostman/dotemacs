;; Milkmacs
;;
;; based on emacs-starter-kit
;; 
;; Updated: 2011-09-20 13:55:00 (dcurtis)
;;
;; 


(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("khealy" . "http://kieranhealy.org/packages/") t)
(add-to-list 'package-archives
             '("elpa-git" . "http://milkbox.net/elpa-git/") t)
(package-initialize)

;; debug if we would like
(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/elisp/")


(require 'uniquify)
(require 'midnight)
(require 'misc)


(push "/usr/local/bin" exec-path)
(push "/usr/texbin" exec-path)
(setenv "PATH"
        (mapconcat 'identity
                   (delete-dups
                    (append (list "/usr/local/bin" "~/bin" "/usr/texbin")
                            (split-string (getenv "PATH") ":")))
                   ":"))


(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
(setq delete-auto-save-files nil)

(savehist-mode)

(ignore-errors
  (server-start))

(setq-default cursor-type '(bar . 1))

(setq user-full-name "Donald Ephraim Curtis")
(setq user-mail-address "dcurtis@milkbox.net")

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq redisplay-dont-pause t)

;; uniquify
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; auto time stamping
(setq time-stamp-active t)
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")


(defalias 'wq 'save-buffers-kill-emacs)
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;; make <C-tab> be M-TAB
(define-key function-key-map (kbd "<C-tab>") (kbd "M-TAB"))


(global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-M-SPC") 'just-one-space)
(global-set-key (kbd "A-h") 'ns-do-hide-emacs)
(global-set-key (kbd "A-M-h") 'ns-do-hide-others)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
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

(define-key ctl-x-4-map "f" 'ido-find-recentfile-other-window)
(global-set-key (kbd "C-x C-d") 'ido-dired)

(global-set-key (kbd "C-c d") 'deft)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-h") (kbd "<backspace>"))
(global-set-key (kbd "C-c h") 'help-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)



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


(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))


(defun delete-this-buffer-and-file ()
  "Deletes current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (delete-file filename)
      (kill-this-buffer))))


(defun rename-this-buffer-and-file ()
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
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))


(defun ido-find-recentfile-other-window ()
  "Find a recent file using ido."
  (interactive)
  (recentf-mode 1)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file-other-window file))))

;; don't quit so easy
(defun close-frame-or-client (&optional args)
  (interactive "P")
  (if (> (length (frame-list)) 1)
      (progn (save-some-buffers)
             (delete-frame))))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (if (eolp) (save-excursion (delete-region (point) (progn (skip-chars-backward " \t") (point)))))
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory)))
  )


(defun marked ()
  "Open the current file in Marked."
  (interactive)
  (when (buffer-file-name)
    (shell-command (concat "open -a Marked " (shell-quote-argument buffer-file-name)))))


(defun make-executable ()
  "Make the current file loaded in the buffer executable"
  (interactive)
  (if (buffer-file-name)
      (shell-command
       (mapconcat 'identity
                  (list "chmod" "u+x" (shell-quote-argument (buffer-file-name))) " "))
    (message "Buffer has no filename.")))

(defun width-80 ()
  (interactive)
  (set-window-margins (selected-window) 0 0)
  (let ((marginwidth (/ (- (window-width) 80) 2)))
    (set-window-margins (selected-window) marginwidth marginwidth)
    )
  )

(defun setup-local-iterm ()
  "locally define C-c C-c to run the iterm-run-previous-command"
  (interactive)
  (local-set-key (kbd "C-c C-c") 'iterm-run-previous-command))

(defun iterm-run-previous-command ()
  "applescript to switch to iTerm and run the previously run command"
  (interactive)
  (save-buffer)
  (call-process "osascript" nil nil nil "-e"
                "
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
      (insert (number-to-string
               (/ (round
                   (* (string-to-number (buffer-substring-no-properties start end)) 1000.0))  1000.0)))
      (delete-region start end)
      )))


(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (newline-and-indent))


(setq auto-mode-alist
      (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))


;; python
(defun python-modes-init ()
  "initialization for all python modes"
  ;; (setup-virtualenv)
  ;; (define-key python-mode-map (kbd "C-h") 'python-indent-dedent-line-backspace)
  (push "~/.virtualenvs/default/bin" exec-path)
  (setenv "PATH"
          (concat
           "~/.virtualenvs/default/bin" ":"
           (getenv "PATH")
           ))

  (font-lock-add-keywords 'python-mode `((,(rx symbol-start (or "import" "from") symbol-end) 0 font-lock-preprocessor-face)))

  (make-face 'font-lock-operator-face)
  (set-face-attribute 'font-lock-operator-face nil :inherit font-lock-keyword-face)
  (setq font-lock-operator-face 'font-lock-operator-face)
  (font-lock-add-keywords 'python-mode `((,(rx symbol-start (or "in" "and" "or" "is" "not") symbol-end) 0 font-lock-operator-face)))

  (add-font-lock-numbers 'python-mode)
  (font-lock-add-keywords
   'python-mode
   `(("^[       ]*\\(@\\)\\([a-zA-Z_][a-zA-Z_0-9.]+\\)\\((.+)\\)?"
      (1 'font-lock-preprocessor-face)
      (2 'font-lock-builtin-face))))
  )

;; (eval-after-load 'python '(python-modes-init))
(eval-after-load 'python-mode '(python-modes-init))

;; yas/snippets
(eval-after-load 'yasnippet
  '(progn
     (yas/load-directory "~/.emacs.d/elpa/yasnippet-2305843009213693951/snippets/")
     (yas/load-directory "~/.emacs.d/snippets/")
     ))

(defun mp-turn-on-yasnippet ()
  (interactive)
  (yas/minor-mode t))

(add-hook 'c-mode-common-hook 'mp-turn-on-yasnippet)
(add-hook 'python-mode-hook 'mp-turn-on-yasnippet)

;; markdown
(defun markdown-pandoc ()
  "process file with pandoc and save it accordingly"
  (interactive)
  (let ((out-buffer-name (concat (file-name-sans-extension (buffer-name)) ".html")))
    (shell-command-on-region
     (point-min) (point-max)
     "~/.cabal/bin/pandoc -H ~/Dropbox/Markdown/antique.css" markdown-output-buffer-name)
    (save-current-buffer
      (set-buffer markdown-output-buffer-name)
      (write-file out-buffer-name)
      (kill-buffer (buffer-name)))))

(eval-after-load 'markdown-mode
  '(progn
     (define-key markdown-mode-map (kbd "C-c m") 'markdown-pandoc)))


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
     (setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
     (setq TeX-view-program-selection '((output-pdf "Skim")))

     (defun TeX-compile ()
       "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
       (interactive)
       (TeX-save-document (TeX-master-file))
       (TeX-command "LaTeX" 'TeX-active-master 0)
       )

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


(defun mp-ido-hook ()
  (setq ido-mode-map ido-completion-map)
  (define-key ido-mode-map "\C-h" 'ido-delete-backward-updir)
  (define-key ido-mode-map "\C-w" 'ido-delete-backward-word-updir)
  (define-key ido-mode-map "\C-n" 'ido-next-match)
  (define-key ido-mode-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map [tab] 'ido-complete)
;;  (ido-everywhere)
  )
(add-hook 'ido-setup-hook 'mp-ido-hook)

(add-hook 'write-file-functions 'time-stamp)

(defun mp-compile ()
  (interactive)
  (save-buffer)
  (compile "make -k"))

(defun mp-add-c-mode-bindings ()
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c C-m") 'mp-compile))

(add-hook 'c-mode-common-hook 'mp-add-c-mode-bindings)

;; paredit
;; (add-hook 'prog-mode-hook 'esk-paredit-nonlisp)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-turn-on-paredit)


;; faces
(make-face 'font-lock-number-face)
(set-face-attribute 'font-lock-number-face nil :inherit font-lock-constant-face)
(setq font-lock-number-face 'font-lock-number-face)
(defvar font-lock-number "[0-9-.]+\\([eE][+-]?[0-9]*\\)?")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
(defun add-font-lock-numbers (mode)
  (font-lock-add-keywords mode
                          `((,(concat "\\<\\(" font-lock-number "\\)\\>" ) 0 font-lock-number-face)
                            (,(concat "\\<\\(" font-lock-hexnumber "\\)\\>" ) 0 font-lock-number-face)
                            )))

;; (font-lock-add-keywords 'emacs-lisp-mode '(("(\\|)\\|'" . 'font-lock-exit-face)))
(font-lock-add-keywords 'emacs-lisp-mode '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
;; (font-lock-add-keywords 'emacs-lisp-mode '(("add-to-list" . font-lock-keyword-face)))
(add-font-lock-numbers 'emacs-lisp-mode)



(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")
       (setenv
        "PYTHONPATH"
        "/Users/dcurtis/src/compepi:/Users/dcurtis/src/networkx")))

(message "done with all but custom")

(if (boundp 'aquamacs-version)
    (setq custom-file "~/.emacs.d/aqustom.el")
  (setq custom-file "~/.emacs.d/custom.el"))
(load custom-file)

(load-file "~/.emacs.d/themes/color-theme-arjen.el")
(color-theme-arjen)


;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
