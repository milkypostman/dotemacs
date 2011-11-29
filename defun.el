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
  (shell-command (concat "open " (shell-quote-argument default-directory)))
  )


(defun marked ()
  "Open the current file in Marked."
  (interactive)
  (when (buffer-file-name)
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


(defun markdown-cleanup-list-numbers-level (&optional pfx)
  "Update the numbering for pfx (as a string of spaces).

Assume that the previously found match was for a numbered item in a list."
  (let ((m pfx)
        (idx 0)
        (success t))
    (while (and success
                (not (string-prefix-p "#" (match-string-no-properties 1)))
                (not (string< (setq m (match-string-no-properties 2)) pfx)))
      (cond
       ((string< pfx m)
        (setq success (markdown-cleanup-list-numbers-level m)))
       (success
        (replace-match
         (concat pfx (number-to-string  (setq idx (1+ idx))) ". "))
        (setq success
              (re-search-forward
               (concat "\\(^#+\\|\\(^\\|^[\s-]*\\)[0-9]+\\. \\)") nil t)))))
    success))


(defun markdown-cleanup-list-numbers ()
  "update the numbering of first-level markdown indexes"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (concat "\\(\\(^[\s-]*\\)[0-9]+\\. \\)") nil t)
      (markdown-cleanup-list-numbers-level (match-string-no-properties 2)))))


(defun shell-command-on-region-to-string (start end command)
  (save-window-excursion
    (with-output-to-string
      (shell-command-on-region start end command standard-output))))


(defun markdown-copy-html ()
  "process file with multimarkdown and save it accordingly"
  (interactive)
  (save-window-excursion
    (flet ((markdown-output-standalone-p () t))
      (markdown))
    (kill-ring-save (point-min) (point-max))))


(defun markdown-copy-rtf ()
  "render and copy as RTF"
  (interactive)
  (save-window-excursion
    (flet ((markdown-output-standalone-p () t))
      (let ((markdown-command (concat markdown-command " -s -t rtf")))
        (message (prin1-to-string (markdown-output-standalone-p)))
        (markdown)
        (shell-command-on-region (point-min) (point-max) "pbcopy")))))


(defun markdown-copy-paste-safari ()
  "process file with multimarkdown, copy it to the clipboard, and
  paste in safari's selected textarea"
  (interactive)
  (markdown-copy-html)
  (do-applescript "
set f to \"~/.emacs.d/osx_edit_md_prevapp\"
set prevapp to do shell script \"touch \" & f & \"; cat \" & f
if prevapp is not \"\" then
tell application prevapp
activate
tell application \"System Events\" to keystroke \"a\" using {command down}
tell application \"System Events\" to keystroke \"v\" using {command down}
end tell
end if"))


     (defun TeX-compile ()
       "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
       (interactive)
       (TeX-save-document (TeX-master-file))
       (TeX-command "LaTeX" 'TeX-active-master 0)
       )


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
  (ido-everywhere)
  )

(defun mp-ido-edit-input ()
  "Edit absolute file name entered so far with ido; terminate by RET.
If cursor is not at the end of the user input, move to end of input."
  (interactive)
  (if (not (eobp))
      (end-of-line)
    (setq ido-text-init ido-text)
    (setq ido-exit 'edit)
    (exit-minibuffer)))

(provide 'defun)

