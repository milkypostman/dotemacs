;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'calendar-mode-abbrev-table '())

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'clojure-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'cssh-mode-abbrev-table '())

(define-abbrev-table 'diary-fancy-display-mode-abbrev-table '())

(define-abbrev-table 'diary-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'docTeX-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("dbest" "Best,
Donald" nil 1)
    ("ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (current-time)))) 4)
    ("demail" "donald-curtis@uiowa.edu" nil 0)
    ("dlove" "Love,
Donald" nil 0)
    ("dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (current-time)))) 0)
    ("dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (current-time)))) 5)
    ("dtdatetime" "" (lambda nil (insert (format-time-string "%Y-%m-%d_%H:%M" (current-time)))) 0)
    ("dthanks" "Thanks,
Donald" nil 0)
    ("dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (current-time)))) 0)
    ("gemail" "dcurtis@gmail.com" nil 0)
    ("hhome" "1105A E PL
Kalona, IA 52247" nil 0)
    ("lol" "haha" nil 0)
    ("memail" "dcurtis@milkbox.net" nil 0)
    ("necissarily" "necessarily" nil 1)
    ("sigce" "--
Donald Ephraim Curtis
donald-curtis@uiowa.edu

CompEpi Group
Department of Computer Science
University of Iowa

" nil 0)
    ("sigd" "--
Donald" nil 0)
    ("sigg" "--
Donald Ephraim Curtis
dcurtis@gmail.com
" nil 0)
    ("sigi" "-Donald

-----
Donald Curtis, Jason Fries, Chris Hlady, Tina McCarty
iScrub Development Team
Computational Epidemiology Group
University of Iowa" nil 1)
    ("sigmb" "--
Donald Ephraim Curtis
dcurtis@milkbox.net" nil 0)
    ("sigui" "--
Donald Ephraim Curtis
donald-curtis@uiowa.edu" nil 0)
    ("tdate" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time 86400))))) 2)
    ("tstamp" "" (lambda nil (insert (format-time-string "%H%M" (current-time)))) 0)
    ("ttime" "" (lambda nil (insert (format-time-string "%H:%M" (current-time)))) 0)
   ))

(define-abbrev-table 'haskell-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'literate-haskell-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-show-branches-mode-abbrev-table '())

(define-abbrev-table 'markdown-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("class" "" python-skeleton-class 0)
    ("def" "" python-skeleton-def 0)
    ("for" "" python-skeleton-for 0)
    ("if" "" python-skeleton-if 0)
    ("try" "" python-skeleton-try 0)
    ("while" "" python-skeleton-while 0)
   ))

(define-abbrev-table 'ruby-mode-abbrev-table '())

(define-abbrev-table 'scala-mode-inf-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'sldb-mode-abbrev-table '())

(define-abbrev-table 'slime-connection-list-mode-abbrev-table '())

(define-abbrev-table 'slime-fuzzy-completions-mode-abbrev-table '())

(define-abbrev-table 'slime-inspector-mode-abbrev-table '())

(define-abbrev-table 'slime-thread-control-mode-abbrev-table '())

(define-abbrev-table 'slime-xref-mode-abbrev-table '())

(define-abbrev-table 'snippet-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'speedbar-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

