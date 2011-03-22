(if window-system
    (progn
      ;; overlay an arrow where the mark is
      (defvar fringemark-overlay-arrow-position)
      (make-variable-buffer-local 'fringemark-overlay-arrow-position)
      ;; (delq 'fringemark-overlay-arrow-position overlay-arrow-variable-list)
      (add-to-list 'overlay-arrow-variable-list  'fringemark-overlay-arrow-position)
      (defun fringemark-mark-hook ()
	;; (make-local-variable 'fringemark-overlay-arrow-position)
	(unless (or (minibufferp (current-buffer)) (not (mark)))
	  (set
	   'fringemark-overlay-arrow-position
	   (save-excursion
	     (goto-char (mark))
	     (forward-line 0)
	     (point-marker)))))
      (add-hook 'post-command-hook 'fringemark-mark-hook)

      ;; make the mark fringe bitmap look cool dude
      (define-fringe-bitmap 'fringemark-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
      (put 'fringemark-overlay-arrow-position 'overlay-arrow-bitmap 'fringemark-hollow-right-arrow)
      ))

(provide 'fringemark)
