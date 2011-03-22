(if window-system
    (progn
      ;; overlay an arrow where the mark is
      (defvar fringe-mark-overlay-arrow-position)
      (make-variable-buffer-local 'fringe-mark-overlay-arrow-position)
      ;; (delq 'fringe-mark-overlay-arrow-position overlay-arrow-variable-list)
      (add-to-list 'overlay-arrow-variable-list  'fringe-mark-overlay-arrow-position)
      (defun fringe-mark-mark-hook ()
	;; (make-local-variable 'fringe-mark-overlay-arrow-position)
	(unless (or (minibufferp (current-buffer)) (not (mark)))
	  (set
	   'fringe-mark-overlay-arrow-position
	   (save-excursion
	     (goto-char (mark))
	     (forward-line 0)
	     (point-marker)))))
      (add-hook 'post-command-hook 'fringe-mark-mark-hook)

      ;; make the mark fringe bitmap look cool dude
      (define-fringe-bitmap 'fringe-mark-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
      (put 'fringe-mark-overlay-arrow-position 'overlay-arrow-bitmap 'fringe-mark-hollow-right-arrow)
      ))

