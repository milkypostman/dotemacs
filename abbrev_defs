;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table '())

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'bookmark-bmenu-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table '())

(define-abbrev-table 'c-mode-abbrev-table '())

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

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'eshell-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("adiminstrator" "administrator" nil 2)
    ("barabasi" "barab\\'{a}si" nil 9)
    ("behaviour" "behavior" nil 4)
    ("ddate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (current-time)))) 5)
    ("densly" "densely" nil 6)
    ("dstamp" "" (lambda nil (insert (format-time-string "%Y%m%d" (current-time)))) 0)
    ("dtdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d %H:%M" (current-time)))) 6)
    ("dtfstamp" "" (lambda nil (insert (format-time-string "%Y-%m-%d-%H%M" (current-time)))) 2)
    ("dtstamp" "" (lambda nil (insert (format-time-string "%Y%m%d%H%M" (current-time)))) 2)
    ("erdos" "Erd\\\"{o}s" nil 14)
    ("existance" "existence" nil 1)
    ("gauruntee" "guarantee" nil 1)
    ("gemail" "dcurtis@gmail.com" nil 0)
    ("guaruntee" "guarantee" nil 3)
    ("hhome" "1105A E PL
Kalona, IA 52247" nil 0)
    ("memail" "dcurtis@milkbox.net" nil 0)
    ("necissarily" "necessarily" nil 3)
    ("renyi" "R\\'{e}nyi" nil 12)
    ("tdate" "" (lambda nil (insert (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time 86400))))) 4)
    ("tstamp" "" (lambda nil (insert (format-time-string "%H%M" (current-time)))) 1)
    ("ttime" "" (lambda nil (insert (format-time-string "%H:%M" (current-time)))) 0)
   ))

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'haskell-mode-abbrev-table '())

(define-abbrev-table 'idea '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'inferior-emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'literate-haskell-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-show-branches-mode-abbrev-table '())

(define-abbrev-table 'markdown-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table '())

(define-abbrev-table 'nxml-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table '())

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table '())

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

(define-abbrev-table 'text-mode-abbrev-table
  '(
    ("GBVC" "\\textbf{GreedyBVC}" nil 3)
    ("\\.\\.\\." "\\dots" nil 0)
    ("kkpsc" "\\textit{$k$-partial set cover}" nil 8)
    ("kkpvc" "\\textit{$k$-partial vertex cover}" nil 27)
    ("node" "vertex" nil 18)
    ("nodes" "vertices" nil 41)
    ("rrdp" "\\textit{restricted diffusion problem}" nil 8)
   ))

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

