;;; defun.el - Milkmacs functions

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
               (message "File '%s' successfully renamed to '%s'"
                        name (file-name-nondirectory new-name))))))))

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(defun mp-paste-previous-osx-app ()
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


(defun mp-copy-paste ()
  "copy the buffer and paste it into the previous buffer or that
  determined by the '.meta' file"
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
    (mp-paste-previous-osx-app)))



(defun close-frame-or-client (&optional args)
  (interactive "P")
  (if (> (length (frame-list)) 1)
      (progn (save-some-buffers)
             (delete-frame))))


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


(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (if (eolp) (save-excursion
               (delete-region (point)
                              (progn (skip-chars-backward " \t") (point)))))
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))


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
      (shell-command
       (combine-and-quote-strings `("chmod" "u+x" ,buffer-file-name)))
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

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (newline-and-indent))


(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))


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
      (2 'font-lock-builtin-face)))))


(defun TeX-compile ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LaTeX" 'TeX-active-master 0))

(defun mp-turn-on-abbrev-mode ()
  "turn on abbrev-mode"
  (abbrev-mode 1))


(defun mp-ibuffer-hook ()
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))

(defun mp-compile ()
  (interactive)
  (save-buffer)
  (compile "make -k"))

(defun mp-add-c-mode-bindings ()
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c C-m") 'mp-compile))


(defun mp-ido-hook ()
  (setq ido-mode-map ido-completion-map)
  (define-key ido-mode-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-mode-map (kbd "C-w") 'ido-delete-backward-word-updir)
  (define-key ido-mode-map (kbd "C-n") 'ido-next-match)
  (define-key ido-mode-map (kbd "C-n") 'ido-next-match)
  (define-key ido-mode-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-mode-map (kbd "C-e") 'mp-ido-edit-input)
  (define-key ido-completion-map [tab] 'ido-complete)
  (ido-everywhere 1))

(defun mp-font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))

(defun mp-c-snug-if (syntax pos)
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

(defun mp-swap-windows ()
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

(defun mp-ido-edit-input ()
  "Edit absolute file name entered so far with ido; terminate by RET.
If cursor is not at the end of the user input, move to end of input."
  (interactive)
  (if (not (eobp))
      (end-of-line)
    (setq ido-text-init ido-text)
    (setq ido-exit 'edit)
    (exit-minibuffer)))

(defun mp-buffer-enable-whitespace-cleanup ()
  "enable whitespace-cleanup in the current buffer"
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))


(defun orgtbl-to-pandoc-cell (val colwidth align)
  "DOCSTRING"
  (setq colwidth (1+ colwidth))
  (if align
      (concat (make-string (- colwidth (length val)) ? ) val)
    (concat val (make-string (- colwidth (length val)) ? ))))


(defun orgtbl-to-pandoc (table params)
  ""
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


(defvar mp-wikipedia-url "http://en.wikipedia.org/wiki/%s" "Wikipedia URL")


(defun mp-wikicase (str)
  "change string to wikipedia case"
  (mapconcat 'capitalize (split-string str) "_"))


(defun mp-markdown-wikipedia-link ()
  "DOCSTRING"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (re-search-forward "\\[\\(.+\\)\\]:" (point-at-eol))
    (end-of-line)
    (insert (format mp-wikipedia-url (mp-wikicase (match-string 1))))))


(defun package-update-all ()
  "Update all packages"
  (interactive)
  (dolist (elt package-alist)
    (let* ((name (car elt))
           (file-name (symbol-name name))
           (available-pkg (assq name package-archive-contents))
           (available-version (and available-pkg
                                   (package-desc-vers (cdr available-pkg))))
           (current-version (package-desc-vers (cdr elt)))
           )
      (when (and available-version
                 (version-list-< current-version available-version))
        (message "Updating to: %s-%s" file-name
                 (package-version-join available-version))
        (package-install name)
        (package-delete file-name (package-version-join current-version))))))


(provide 'defun)
