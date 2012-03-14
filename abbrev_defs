;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'bookmark-bmenu-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'calendar-mode-abbrev-table '())

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'clojure-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'cssh-mode-abbrev-table '())

(define-abbrev-table 'custom-new-theme-mode-abbrev-table '())

(define-abbrev-table 'custom-theme-choose-mode-abbrev-table '())

(define-abbrev-table 'diary-fancy-display-mode-abbrev-table '())

(define-abbrev-table 'diary-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'docTeX-mode-abbrev-table '())

(define-abbrev-table 'doctex-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'eshell-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'gfm-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("10ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 10 86400)))))) 4)
    ("10dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 10 86400)))))) 4)
    ("10dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 10 86400)))))) 4)
    ("10dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 10 86400)))))) 4)
    ("10dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 10 86400)))))) 4)
    ("11ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 11 86400)))))) 4)
    ("11dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 11 86400)))))) 4)
    ("11dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 11 86400)))))) 4)
    ("11dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 11 86400)))))) 4)
    ("11dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 11 86400)))))) 4)
    ("12ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 12 86400)))))) 4)
    ("12dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 12 86400)))))) 4)
    ("12dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 12 86400)))))) 4)
    ("12dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 12 86400)))))) 4)
    ("12dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 12 86400)))))) 4)
    ("13ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 13 86400)))))) 4)
    ("13dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 13 86400)))))) 4)
    ("13dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 13 86400)))))) 4)
    ("13dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 13 86400)))))) 4)
    ("13dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 13 86400)))))) 4)
    ("14ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 14 86400)))))) 4)
    ("14dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 14 86400)))))) 4)
    ("14dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 14 86400)))))) 4)
    ("14dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 14 86400)))))) 4)
    ("14dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 14 86400)))))) 4)
    ("1ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 1 86400)))))) 4)
    ("1dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 1 86400)))))) 4)
    ("1dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 1 86400)))))) 4)
    ("1dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 1 86400)))))) 4)
    ("1dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 1 86400)))))) 4)
    ("2ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 2 86400)))))) 4)
    ("2dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 2 86400)))))) 5)
    ("2dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 2 86400)))))) 4)
    ("2dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 2 86400)))))) 4)
    ("2dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 2 86400)))))) 4)
    ("3ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 3 86400)))))) 5)
    ("3dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 3 86400)))))) 5)
    ("3dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 3 86400)))))) 4)
    ("3dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 3 86400)))))) 4)
    ("3dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 3 86400)))))) 4)
    ("4ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 4 86400)))))) 4)
    ("4dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 4 86400)))))) 5)
    ("4dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 4 86400)))))) 4)
    ("4dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 4 86400)))))) 4)
    ("4dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 4 86400)))))) 4)
    ("5ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 5 86400)))))) 4)
    ("5dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 5 86400)))))) 4)
    ("5dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 5 86400)))))) 4)
    ("5dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 5 86400)))))) 4)
    ("5dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 5 86400)))))) 4)
    ("6ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 6 86400)))))) 4)
    ("6dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 6 86400)))))) 4)
    ("6dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 6 86400)))))) 4)
    ("6dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 6 86400)))))) 4)
    ("6dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 6 86400)))))) 4)
    ("7ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 7 86400)))))) 5)
    ("7dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 7 86400)))))) 5)
    ("7dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 7 86400)))))) 4)
    ("7dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 7 86400)))))) 4)
    ("7dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 7 86400)))))) 4)
    ("8ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 8 86400)))))) 4)
    ("8dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 8 86400)))))) 4)
    ("8dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 8 86400)))))) 4)
    ("8dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 8 86400)))))) 4)
    ("8dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 8 86400)))))) 4)
    ("9ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (* 9 86400)))))) 4)
    ("9dfull" "" (lambda nil (insert (format-time-string "%A, %B %d, %Y" (time-add (current-time) (seconds-to-time (* 9 86400)))))) 4)
    ("9dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (time-add (current-time) (seconds-to-time (* 9 86400)))))) 4)
    ("9dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) (seconds-to-time (* 9 86400)))))) 4)
    ("9dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (time-add (current-time) (seconds-to-time (* 9 86400)))))) 4)
    ("adiminstrator" "administrator" nil 2)
    ("behaviour" "behavior" nil 4)
    ("ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (current-time)))) 6)
    ("ddue" "" (lambda nil (insert (format-time-string "%A, %B %d, 11:59pm" (current-time)))) 1)
    ("densly" "densely" nil 6)
    ("dfull" "" (lambda nil (insert (format-time-string "%A, %B %d,%Y" (current-time)))) 3)
    ("dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (current-time)))) 0)
    ("dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (current-time)))) 6)
    ("dtfn" "" (lambda nil (insert (format-time-string "%Y-%m-%d-%H%M" (current-time)))) 2)
    ("dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (current-time)))) 2)
    ("emgm" "dcurtis@gmail.com" nil 0)
    ("emmb" "dcurtis@milkbox.net" nil 0)
    ("existance" "existence" nil 1)
    ("gauruntee" "guarantee" nil 1)
    ("guaruntee" "guarantee" nil 3)
    ("hhome" "701 Caroline Ave.
Iowa City, IA 52245" nil 0)
    ("memail" "dcurtis@milkbox.net" nil 0)
    ("necissarily" "necessarily" nil 3)
    ("tstamp" "" (lambda nil (insert (format-time-string "%H%M" (current-time)))) 1)
    ("ttime" "" (lambda nil (insert (format-time-string "%H:%M" (current-time)))) 0)
   ))

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'haskell-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'html-mode-abbrev-table '())

(define-abbrev-table 'idea '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'inferior-emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'latex-mode-abbrev-table '())

(define-abbrev-table 'lisp-interaction-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'literate-haskell-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-branch-manager-mode-abbrev-table '())

(define-abbrev-table 'magit-commit-mode-abbrev-table '())

(define-abbrev-table 'magit-diff-mode-abbrev-table '())

(define-abbrev-table 'magit-log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-log-mode-abbrev-table '())

(define-abbrev-table 'magit-reflog-mode-abbrev-table '())

(define-abbrev-table 'magit-show-branches-mode-abbrev-table '())

(define-abbrev-table 'magit-stash-mode-abbrev-table '())

(define-abbrev-table 'magit-status-mode-abbrev-table '())

(define-abbrev-table 'magit-wazzup-mode-abbrev-table '())

(define-abbrev-table 'markdown-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table '())

(define-abbrev-table 'nxml-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'plain-tex-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("class" "" python-skeleton-class 0)
    ("def" "" python-skeleton-def 0)
    ("for" "" python-skeleton-for 0)
    ("if" "" python-skeleton-if 0)
    ("pyimp" "import os
import os.path
import uihc
import networkx as nx" nil 0)
    ("try" "" python-skeleton-try 0)
    ("while" "" python-skeleton-while 0)
   ))

(define-abbrev-table 'reftex-index-mode-abbrev-table '())

(define-abbrev-table 'reftex-index-phrases-mode-abbrev-table '())

(define-abbrev-table 'reftex-select-bib-mode-abbrev-table '())

(define-abbrev-table 'reftex-select-label-mode-abbrev-table '())

(define-abbrev-table 'reftex-toc-mode-abbrev-table '())

(define-abbrev-table 'ruby-mode-abbrev-table '())

(define-abbrev-table 'scala-mode-inf-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sgml-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'sldb-mode-abbrev-table '())

(define-abbrev-table 'slime-connection-list-mode-abbrev-table '())

(define-abbrev-table 'slime-fuzzy-completions-mode-abbrev-table '())

(define-abbrev-table 'slime-inspector-mode-abbrev-table '())

(define-abbrev-table 'slime-thread-control-mode-abbrev-table '())

(define-abbrev-table 'slime-xref-mode-abbrev-table '())

(define-abbrev-table 'slitex-mode-abbrev-table '())

(define-abbrev-table 'snippet-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'speedbar-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'tar-mode-abbrev-table '())

(define-abbrev-table 'tex-mode-abbrev-table '())

(define-abbrev-table 'tex-shell-abbrev-table '())

(define-abbrev-table 'texinfo-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table
  '(
    ("GBVC" "\\textbf{GreedyBVC}" nil 3)
    ("\\.\\.\\." "\\dots" nil 0)
    ("barabasi" "barab\\'{a}si" nil 9)
    ("erdos" "Erd\\\"{o}s" nil 14)
    ("kkpsc" "\\textit{$k$-partial set cover}" nil 8)
    ("kkpvc" "\\textit{$k$-partial vertex cover}" nil 27)
    ("renyi" "R\\'{e}nyi" nil 12)
    ("rrdp" "\\textit{restricted diffusion problem}" nil 8)
    ("trm" "\\textrm" nil 6)
   ))

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

