;; ;; python-mode.el
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;; 				   interpreter-mode-alist))
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
  (ignore-errors (pymacs-load "ropemacs" "rope-")

		 ;; (setq ropemacs-codeassist-maxfixes 3)
		 (setq ropemacs-guess-project t)
		 (setq ropemacs-enable-autoimport t)

		 ;; (add-hook 'python-mode-hook
		 ;; 	   (lambda ()
		 ;; 	     (cond ((file-exists-p ".ropeproject")
		 ;; 		    (rope-open-project default-directory))
		 ;; 		   ((file-exists-p "../.ropeproject")
		 ;; 		    (rope-open-project (concat default-directory "..")))
		 ;; 		   )))
		 ))

;; python.el by fabia'n
;; (add-to-list 'load-path "~/.emacs.d/vendor/python.el")
;; (require 'python)

;;(autoload 'python-mode "python" "Python editing mode." t)
(eval-after-load 'python
  '(progn
     (setup-ropemacs)
     (setup-virtualenv)
     ))

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


(defvar flymake-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-p" 'flymake-goto-prev-error)
    (define-key map "\M-n" 'flymake-goto-next-error)
    map)
  "Keymap for my flymake minor mode.")


(provide 'milkmacs-python)
