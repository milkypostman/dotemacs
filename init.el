;; Milkmacs
;;
;; Simple setup for Python and other things.
;; Autocompletion is setup automatically.
;; To complete using Rope completion hit M-/
;;
;; Updated: 2011-06-22 12:54:51 (dcurtis)
;;
;; the following command should be run manually ever once and a while.
;; (byte-recompile-directory "~/.emacs.d/elisp/" 0 t)
;; (byte-recompile-directory "~/.emacs.d/elisp/python" 0 t)
;; (save-buffers-kill-emacs)

;; basic configuration
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(message "milkmacs")


(message "milkmacs: setting up path")
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; aquamacs stuff
(when (boundp 'aquamacs-version)
  (tabbar-mode -1)
  (one-buffer-one-frame-mode -1)
  (setq special-display-regexps nil))


;; debug if we would like
(setq debug-on-error t)

;; properly setup the environment
(push "/usr/local/bin" exec-path)

(setenv "PATH"
        (mapconcat 'identity
                   (delete-dups
                    (append (list "/usr/local/bin" "~/bin" "/usr/texbin")
                            (split-string (getenv "PATH") ":")))
                   ":"))



;; Load all of my plugins
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/elisp/")


(ignore-errors
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  ;; (add-to-list 'package-archives '("kieranhealy" . "http://kieranhealy.org/packages/") t)
  ;; (add-to-list 'package-archives '("josh" . "http://josh.github.com/elpa/") t)

  ;; required for cssh
  (package-initialize))

(setq user-full-name "Donald Ephraim Curtis")
(setq user-mail-address "dcurtis@milkbox.net")

;; my function for adding all vendor specific directories (not
;; subdirectories) to the load-path and put them first!
(defun add-subdirs-load-path (default-directory)
  (let* ((dirs (directory-files default-directory)))
    (dolist (dir dirs)
      (unless (member dir '("." ".." "RCS" "CVS" "rcs" "cvs"))
        (let ((fullpath (concat default-directory dir)))
          (when (file-directory-p dir)
            (add-to-list 'load-path fullpath)))))))

(defun add-subdirs-theme-path (default-directory)
  (let* ((dirs (directory-files default-directory)))
    (dolist (dir dirs)
      (unless (member dir '("." ".." "RCS" "CVS" "rcs" "cvs"))
        (let ((fullpath (concat default-directory dir)))
          (when (file-directory-p dir)
            (add-to-list 'custom-theme-load-path fullpath)))))))

(add-subdirs-load-path "~/.emacs.d/elisp/")
(add-subdirs-load-path "~/.emacs.d/themes/")

(ignore-errors
  (add-subdirs-theme-path "~/.emacs.d/themes/")
  (setq custom-theme-directory "~/.emacs.d/themes/"))
;; (add-to-list 'load-path "~/.emacs.d/elisp/slime/contrib/")

;; do we want VIM mode?
;; (require 'vimpulse)

(message "milkmacs: starting server")

;; start the server
;; (setq server-use-tcp t)
(server-start)


(message "milkmacs: setting settings")

; save minibuffer history across sessions
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode 1)

;; don't be poppin' new frames
(setq ns-pop-up-frames nil)

;; use default Mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

(global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-M-SPC") 'just-one-space)

;; don't confirm opening non-existant files/buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; yes, I want to kill buffers with processes attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

; nicer naming of buffers with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; auto time stamping
(setq time-stamp-active t)
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")
(add-hook 'write-file-functions 'time-stamp)

;; backup settings
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq default-abbrev-mode nil)

(setq-default cursor-type 'bar)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
(setq delete-auto-save-files nil)

(setq recentf-save-file "~/.emacs.d/recentf")

(setq default-indicate-buffer-boundaries (quote left))

(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)

;; try it as the default
(setq ns-alternate-modifier 'super)
(setq ns-command-modifier 'meta)


(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab))

(setq line-number-mode t)
(setq column-number-mode t)
(setq set-mark-command-repeat-pop t)

(set-default 'indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")
(setq tramp-default-method "rsync")

(delete-selection-mode 1)
(global-auto-revert-mode 1)

(setq comint-prompt-read-only t)
;; (require 'ansi-color)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-to-list 'auto-mode-alist '("\\.bashrc_.*" . sh-mode))

;; keybindings
(message "milkmacs: binding bindings")

;; make <C-tab> be M-TAB
(define-key function-key-map (kbd "<C-tab>") (kbd "M-TAB"))

;; don't quit so easy
(defun close-frame-or-client (&optional args)
  (interactive "P")
  (if (> (length (frame-list)) 1)
      (progn (save-some-buffers)
             (delete-frame))))
(global-set-key (kbd "C-x C-c") 'close-frame-or-client)
(defalias 'wq 'save-buffers-kill-emacs)

;; autoindent
(global-set-key (kbd "RET") 'newline-and-indent)

;; make ctrl-w work as expected
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-z") 'other-window)
(global-set-key (kbd "C-M-,") 'beginning-of-buffer-other-window)
(global-set-key (kbd "M-`") 'other-frame)

;; hack alternative for M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-unset-key (kbd "C-x m")) ; disable mail

;; remap the delete key, who needs help?
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-c h") 'help-command)

;; misc commands stolen from the starter kit
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-4") 'ctl-x-4-prefix)
(global-set-key (kbd "C-.") 'repeat)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
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

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-M-;") 'comment-dwim-line)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-x m") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)

(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-C-r") 'replace-regexp)

(global-set-key (kbd "M-C-m")   'open-next-line)
;; (global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

;; misc useful functions
;; http://www.emacswiki.org/cgi-bin/wiki/misc-cmds
(autoload 'beginning-or-indentation "misc-cmds")
(global-set-key "\C-a" 'beginning-or-indentation)

(eval-after-load "term"
  '(progn
     (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")

     ;; allow M-` to switch windows in terminal raw mode.
     (define-key term-raw-map (kbd "M-`") nil)
     (define-key term-raw-map (kbd "C-z") nil)
     ;; (add-hook 'term-setup-hook (lambda () (setq truncate-lines t)))
     ;; (setq term-default-bg-color (frame-parameter nil 'background-color))
     ;; (setq term-default-fg-color (frame-parameter nil 'foreground-color))
     (setq term-default-bg-color nil)
     (setq term-default-fg-color nil)
     ;; (define-key term-raw-map (kbd "C-c") 'mode-specific-command-prefix)
     ;; (define-key term-raw-map (kbd "C-c C-c") 'term-send-raw)
     (define-key term-raw-map (kbd "C-x") 'Control-X-prefix)))

    ;; (define-key term-raw-map (kbd "C-x C-z") 'term-send-raw)))


(autoload 'multi-term "multi-term" "multiple terms" t)


(eval-after-load "dired"
  '(define-key dired-mode-map "F" 'dired-find-file-other-frame))


;; defun
(message "milkmacs: defuning functions")

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
tell application \"iTerm\"
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

(defun unpop-to-mark-command ()
  "Unpop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (interactive)
  (let ((num-times (if (equal last-command 'pop-to-mark-command) 2
                     (if (equal last-command 'unpop-to-mark-command) 1
                       (error "Previous command was not a (un)pop-to-mark-command")))))
    (dotimes (x num-times)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (+ 0 (car (last mark-ring))) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (mark t)))
      (deactivate-mark))))

(global-set-key (kbd "C-c C-SPC") 'unpop-to-mark-command)


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

(defun dired-find-file-other-frame ()
  "In Dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))


;; emacsclient map
(defun server-edit-save ()
  "Save the file and exit server mode."
  (interactive)
  (save-buffer)
  (server-edit))

;; (add-hook 'server-visit-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-c c") 'server-edit-save)))

;; automatically recursive search
;; (defadvice isearch-search (after isearch-no-fail activate)
;;   (unless isearch-success
;;     (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)
;;     (isearch-repeat (if isearch-forward 'forward))
;;     (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)
;;     ))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-'") 'push-mark-no-activate)

(defun exchange-point-and-mark-no-activate (&optional arg)
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive "P")
  (let ((region-active (region-active-p)))
    (exchange-point-and-mark)
    (if (or arg region-active) (activate-mark)
      (deactivate-mark))))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark)
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defadvice jump-to-register (before jump-to-register-advice activate)
  (push-mark (point) t nil))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun special-open-line (n)
  (interactive "p")
  (let ((before-indent
         (buffer-substring-no-properties
          (point) (save-excursion (skip-chars-backward "\t ") (point))))
        (after-indent
         (buffer-substring-no-properties
          (point) (save-excursion (skip-chars-forward "\t ") (point)))))
    (if (not (bolp))
        (let ((in-the-gut (save-excursion (skip-chars-backward "\t ") (bolp))))
          (save-excursion
            (while (> n 0)
              (newline)
              (setq n (1- n)))
            (cond ((not in-the-gut) (indent-according-to-mode))
                  ((bolp) (insert before-indent))))
          (if in-the-gut (insert after-indent)))
      (save-excursion (open-line n))
      (insert after-indent))
    ))

(global-set-key (kbd "C-o") 'special-open-line)


;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (newline-and-indent))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (if (eolp) (save-excursion (delete-region (point) (progn (skip-chars-backward " \t") (point)))))
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

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

(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " default-directory))
  )

(defun make-executable ()
  "Make the current file loaded in the buffer executable"
  (interactive)
  (if (buffer-file-name)
      (shell-command
       (mapconcat 'identity
                  (list "chmod" "u+x" (shell-quote-argument (buffer-file-name))) " "))
    (message "Buffer has no filename.")))


;; elisp
(message "milkmacs: loading elisp packages")

;; winring
;; (require 'winring)
;; (setq winring-keymap-prefix (kbd "C-\\"))
;; (global-set-key (kbd "C-\\ c") 'winrigh-new-configuration)
;; (global-set-key (kbd "C-\\ C-n") (kbd "C-\\ n"))
;; (global-set-key (kbd "C-\\ n") 'winring-next-configuration)
;; (global-set-key (kbd "C-\\ C-p") (kbd "C-\\ p"))
;; (winring-initialize)

;; workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-\\"))
(workgroups-mode 1)
(global-set-key (kbd "C-\\ C-\\") 'wg-switch-to-previous-workgroup)
;; (wg-load "~/.emacs.d/workgroups")

; nicer naming of buffers with identical names
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'reverse)
;; (setq uniquify-separator " • ")
;; (setq uniquify-after-kill-buffer-p t)
;; (setq uniquify-ignore-buffers-re "^\\*")

(require 'sentence-highlight-mode)



;; org-mode
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-archive-location "~/Dropbox/Notational/zarchive.org::* %s")
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-completion-use-ido t)
(setq org-directory "~/Dropbox/Notational")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notational/dox inbox.org" "Incoming")
         "* TODO %?\n  %i\n  %a")))
(setq org-agenda-files (list "~/Dropbox/Notational"))
(setq org-refile-targets (quote ((nil :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 2))))
(setq org-refile-use-outline-path (quote file))
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Notational/dox inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")


;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))


;; save place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

(require 'midnight)
(require 'misc)

;; move expand-line to the end
(setq hippie-expand-try-functions-list (delq 'try-expand-line hippie-expand-try-functions-list))
(add-to-list 'hippie-expand-try-functions-list 'try-expand-line t)

(defun fancy-tab (arg)
  (interactive "P")
  (setq this-command last-command)
  (if (or (eq this-command 'hippie-expand) (looking-at "\\_>"))
      (progn
        (setq this-command 'hippie-expand)
        (hippie-expand arg))
    (setq this-command 'indent-for-tab-command)
    (indent-for-tab-command arg)))


(define-key read-expression-map (kbd "M-/") 'hippie-expand)
;; (global-set-key (kbd "TAB") 'fancy-tab)
(global-set-key (kbd "TAB") 'indent-for-tab-command)
(global-set-key (kbd "M-/") 'hippie-expand)




(add-hook 'emacs-lisp-mode-hook 'move-lisp-completion-to-front)
(defun move-lisp-completion-to-front ()
  "Adjust hippie-expand-try-functions-list to have lisp completion at the front."
  (make-local-variable 'hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list
        (append (list 'yas/hippie-try-expand 'try-complete-lisp-symbol-partially 'try-complete-lisp-symbol)
                (delq 'yas/hippie-try-expand
                      (delq 'try-complete-lisp-symbol-partially
                            (delq 'try-complete-lisp-symbol
                                  hippie-expand-try-functions-list))))))

;; ido
(require 'ido)

;; these must be set *before* enabling ido mode
(setq ido-default-buffer-method 'selected-window)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")

(ido-mode t)
(ido-everywhere t)


(add-hook 'ido-setup-hook
          (lambda ()
            (setq ido-mode-map ido-completion-map)
            (define-key ido-mode-map "\C-h" 'ido-delete-backward-updir)
            (define-key ido-mode-map "\C-w" 'ido-delete-backward-word-updir)
            (define-key ido-mode-map "\C-n" 'ido-next-match)
            (define-key ido-mode-map "\C-p" 'ido-prev-match)
            (define-key ido-completion-map [tab] 'ido-complete)
            ))

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))


;; http://www.emacswiki.org/emacs/idomenu
(autoload 'idomenu "idomenu" nil t)

;; ido recent files

(global-set-key (kbd "C-x f") 'ido-find-recentfile)
(global-set-key (kbd "C-c n") '(lambda () (interactive) (ido-find-file-in-dir "~/Dropbox/Notational")))
(global-set-key (kbd "M-.") 'ido-find-tag)
(define-key ctl-x-4-map "f" 'ido-find-recentfile-other-window)
(global-set-key (kbd "C-x C-i") 'idomenu)
(global-set-key (kbd "C-x C-d") 'ido-dired)


(defun ido-find-recentfile ()
  "Find a recent file using ido."
  (interactive)
  (recentf-mode 1)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun ido-find-recentfile-other-window ()
  "Find a recent file using ido."
  (interactive)
  (recentf-mode 1)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file-other-window file))))

(defun ido-find-tag ()
  "Find a tag using ido."
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-yank ()
  (interactive)
  (insert-for-yank
   (ido-completing-read "Select kill: " kill-ring)))

;; yasnippet -- really slow so don't load it less we're on the desktop
(when (require 'yasnippet nil 'noerror)
  (yas/initialize)
  (setq yas/root-directory
        '("~/.emacs.d/elisp/yasnippet/snippets"
          "~/.emacs.d/snippets")) ;; my own snippets
  (mapc 'yas/load-directory yas/root-directory)
  (setq yas/wrap-around-region t)
  (setq yas/prompt-functions
        '(yas/x-prompt yas/ido-prompt))
  (yas/global-mode 1) ;;  make it global
  (add-to-list 'auto-mode-alist '("yas/.*" . snippet-mode)))


;; magit
(autoload 'magit-status "magit" "MaGIT")
(global-set-key (kbd "C-x g") 'magit-status)

;; git-commit-mode
(require 'git-commit)
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))

;; auctex
(eval-after-load 'latex
  '(progn
     (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
     (setq TeX-source-correlate-method 'synctex)


     (setq TeX-auto-save t)
     (setq TeX-parse-self t)
     (setq TeX-save-query nil)
     (setq-default TeX-PDF-mode t)
     ;; (setq-default TeX-master nil)
     ;; (setq LaTeX-command "latex")
     (setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
     (setq TeX-view-program-selection '((output-pdf "Skim")))
     (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     (setq reftex-plug-into-AUCTeX t)
     (define-key TeX-mode-map (kbd "C-c C-m") 'TeX-command-master)
     (define-key TeX-mode-map (kbd "C-c C-c")
       (lambda ()
         (interactive)
         (TeX-save-document (TeX-master-file))
         (TeX-command "LaTeX" 'TeX-master-file)
         ))
     ))

(eval-after-load 'reftex
  '(progn
     (add-to-list 'reftex-section-prefixes '(1 . "chap:"))))

;; disable vc
(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;; cssh
;; (require 'cssh)


;; erlang -- FIXME
;;(require 'erlang-start)

;; clojure
(require 'clojure-mode)
(require 'slime)
(slime-setup '(slime-repl))
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; scala
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
          (lambda ()
            (yas/load-directory "~/.emacs.d/elisp/scala-mode/contrib/yasnippet/snippets")
            ))

;; haskell
(load "~/.emacs.d/elisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; R
;; manually do autoloads so the whole shebang doesn't load everytime.
;; I hardly use R.
;; (require 'ess-site)
(add-to-list 'load-path "~/.emacs.d/elisp/ess/lisp/")
(autoload 'R-mode "ess-site" "R mode")
(autoload 'R "ess-site" "R inferiors hell" t)
(setq auto-mode-alist
      (cons '("\\.[rR]\\'"      . R-mode) auto-mode-alist))


;; markdown
;; use autoload because it delays loading the function until we need it.
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook '(lambda ()
                                 (auto-fill-mode 1)
                                 (make-local-variable 'before-save-hook)
                                 (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
                                 ))
;; python
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                    interpreter-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(defun setup-virtualenv ()
  "Setup virtualenv"
  (ignore-errors
    ;; setup virtualenv for the python-shell-process-environment
    (push "~/.virtualenvs/default/bin" exec-path)
    (setenv "PATH"
            (concat
             "~/.virtualenvs/default/bin" ":"
             (getenv "PATH")
             ))
    (add-hook 'python-mode-hook
              (lambda ()
                (setq python-shell-process-environment
                      (list
                       (format "PATH=%s" (mapconcat
                                          'identity
                                          (reverse
                                           (cons (getenv "PATH")
                                                 '("~/.virtualenvs/default/bin/")))
                                          ":"))
                       "VIRTUAL_ENV=~/.virtualenvs/default/"))
                (setq python-shell-exec-path '("~/.virtualenvs/default/bin/"))))
    ))

(defun setup-ropemacs ()
  "Setup ropemacs"
  (with-temp-buffer
    (cd "~")
    (pymacs-load "ropemacs" "rope-"))
  ;; (ignore-errors (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)
  )

(eval-after-load 'pymacs
  (setq pymacs-python-command "/Users/dcurtis/.virtualenvs/default/bin/python")
  )


(defun python-modes-init ()
  "initialization for all python modes"
     (setup-virtualenv)
     (define-key python-mode-map (kbd "C-h") 'python-indent-dedent-line-backspace)
     (font-lock-add-keywords 'python-mode `((,(rx symbol-start (or "import" "from") symbol-end) 0 font-lock-preprocessor-face)))

     (make-face 'font-lock-operator-face)
     (set-face-attribute 'font-lock-operator-face nil :inherit font-lock-keyword-face)
     (setq font-lock-operator-face 'font-lock-operator-face)
     (font-lock-add-keywords 'python-mode `((,(rx symbol-start (or "in" "and" "or" "is" "not") symbol-end) 0 font-lock-operator-face)))

     (add-font-lock-numbers 'python-mode)
     (font-lock-add-keywords
      'python-mode
      `(("^[ 	]*\\(@\\)\\([a-zA-Z_][a-zA-Z_0-9.]+\\)\\((.+)\\)?"
         (1 'font-lock-preprocessor-face)
         (2 'font-lock-builtin-face))))
)

(eval-after-load 'python '(python-modes-init))
(eval-after-load 'python-mode '(python-modes-init))

;; (require 'auto-complete)
;; (defvar ac-ropemacs-last-candidates)
;; (defun ac-ropemacs-candidates ()
;;   (setq ac-ropemacs-last-candidates ())
;;   (mapcar (lambda (item)
;;             (let ((name (car item))
;;                   (doc (cadr item))
;;                   (type (caddr item)))
;;               (add-to-list 'ac-ropemacs-last-candidates
;;                            (cons (concat ac-prefix name) doc))
;;               (concat ac-prefix name)))
;;           (rope-extended-completions)))

;; (defun ac-ropemacs-document (name)
;;   (let ((item (assoc name ac-ropemacs-last-candidates)))
;;     (if item (cdr item))))

;; (ac-define-source nropemacs
;;   '((candidates . ac-ropemacs-candidates)
;;     (symbol     . "p")
;;     (document   . ac-ropemacs-document)
;;     (cache      . t)))

;; (ac-define-source nropemacs-dot
;;   '((candidates . ac-ropemacs-candidates)
;;     (symbol     . "p")
;;     (document   . ac-ropemacs-document)
;;     (cache      . t)
;;     (prefix     . c-dot)
;;     (requires   . 0)))

;; (defun ac-python-mode-setup ()
;;   (setq ac-sources
;;         '(ac-source-nropemacs ac-source-nropemacs-dot ac-source-yasnippet)))

;; (defun ac-self-insert-and-complete ()
;;   (interactive)
;;   (self-insert-command 1)
;;   (ac-start))

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)


(add-hook 'before-save-hook 'delete-trailing-whitespace nil nil)

(eval-after-load 'flymake
  '(progn
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "epylint" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init))))

(require 'flymake-cursor)

(defadvice flymake-mode (after post-command-stuff activate compile)
  "add keybindings"
  (local-set-key "\M-p" 'flymake-goto-prev-error)
  (local-set-key "\M-n" 'flymake-goto-next-error))


(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))


;; ruby

;; (autoload 'run-ruby "inf-ruby"
;;   "Run an inferior Ruby process")
;; (autoload 'inf-ruby-keys "inf-ruby"
;;   "Set local key defs for inf-ruby in ruby-mode")
;; (add-hook 'ruby-mode-hook
;;        '(lambda ()
;;           (inf-ruby-keys)
;;           ))
;; (add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; (eval-after-load 'ruby-mode
;;   '(progn
;;      (define-key ruby-mode-map (kbd "C-c C-c") 'ruby-run-w/compilation)
;;      ))



(make-face 'font-lock-number-face)
(set-face-attribute 'font-lock-number-face nil :inherit font-lock-constant-face)
(setq font-lock-number-face 'font-lock-number-face)
(defvar font-lock-number "[0-9-.]+\\([eE][+-]?[0-9]*\\)?")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
(defun add-font-lock-numbers (mode)
  (font-lock-add-keywords mode `(
                                 (,(concat "\\<\\(" font-lock-number "\\)\\>" ) 0 font-lock-number-face)
                                 (,(concat "\\<\\(" font-lock-hexnumber "\\)\\>" ) 0 font-lock-number-face)
                                 )))

(font-lock-add-keywords 'emacs-lisp-mode '(("(\\|)\\|'" . 'font-lock-exit-face)))
(font-lock-add-keywords 'emacs-lisp-mode '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
;; (font-lock-add-keywords 'emacs-lisp-mode '(("add-to-list" . font-lock-keyword-face)))
(add-font-lock-numbers 'emacs-lisp-mode)

(if (not (window-system))
    (menu-bar-mode 0)
  (require 'fringemark)
  (fringe-mode '(1 . 0))
  (setq mouse-wheel-scroll-amount '(0.0001))
  )


(show-paren-mode 1)

(require 'color-theme)
(setq color-theme-is-global nil)
;; (require 'color-theme-ir-black)
(load "~/.emacs.d/themes/color-theme-ir-black.el")
(load "~/.emacs.d/themes/color-theme-dirac.el")
(load "~/.emacs.d/themes/color-theme-mac-classic.el")
;; (load "~/.emacs.d/themes/color-theme-gruber-darker.el")


;; custom stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#002b36")
 '(background-mode dark)
 '(cursor-color "#839496")
 '(custom-enabled-themes nil)
 '(custom-safe-themes (quote ("5f5644eaf825f7ef4a7f8137540821a3a2ca009e" "aa1610894e3435eabcb008a7b782fbd83d1a3082" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "0174d99a8f1fdc506fa54403317072982656f127" default)))
 '(delete-selection-mode t)
 '(file-name-shadow-mode nil)
 '(foreground-color "#839496")
 '(line-spacing 0.2)
 '(ns-antialias-text t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(url-handler-mode t)
 '(wg-restore-position t)
 '(yas/wrap-around-region t))


;; System Specific Settings
(cond ((eq system-type 'darwin)
       (setenv "PYTHONPATH" "/Users/dcurtis/Development/compepi:/Users/dcurtis/Development/networkx")))


(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Anonymous_Pro"))))
 '(sentence-face ((t (:inherit minibuffer-prompt))) t))




;; (add-to-list 'default-frame-alist '(height . 71))
;; (add-to-list 'default-frame-alist '(width . 158))

;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
