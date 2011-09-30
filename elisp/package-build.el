;;; package-build.el --- Tools for curating the package archive

;; Copyright (C) 2011 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Created: 2011-09-30
;; Version: 0.1
;; Keywords: tools

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file allows a curator to publish an archive of Emacs packages.

;; The archive is generated from an index, which contains a list of
;; projects and repositories from which to get them. The term
;; "package" here is used to mean a specific version of a project that
;; is prepared for download and installation.

;; Currently only supports single-file projects stored in git.

;;; Code:

;; Since this library is not meant to be loaded by users
;; at runtime, use of cl functions should not be a problem.
(require 'cl)

(require 'package)

(defvar package-build-working-dir "~/.emacs.d/elisp/working/"
  "Directory in which to keep checkouts.")

(defvar package-build-archive-dir "~/.emacs.d/elisp/archives/"
  "Directory in which to keep compiled archives.")

(defun package-build-checkout-svn (repo dir) 
  "checkout an svn package"
  (process-file
   "svn" nil
   (get-buffer-create "*package-build-checkout*") nil "checkout" (concat repo "/trunk") dir))

(defun package-build-checkout-svn (repo dir) 
  "checkout an svn repo"
  (process-file
   "svn" nil
   (get-buffer-create "*package-build-checkout*") nil "checkout" (concat repo "/trunk") dir))

(defun package-build-checkout-git (repo dir) 
  "checkout an git repo"
  (process-file
   "git" nil
   (get-buffer-create "*package-build-checkout*") nil "clone" repo dir))

(defun package-build-pkg-file (pkg-file file-name version homepage)
  "build the pkg file"
  (let ((print-level nil)
        (print-length nil)
        (requires nil))
    (write-region
     (concat
      (prin1-to-string
       (list 'define-package
             file-name
             version
             homepage
             (list 'quote
                   ;; Turn version lists into string form.
                   (mapcar
                    (lambda (elt)
                      (list (car elt)
                            (package-version-join (cadr elt))))
                    requires))
             ))
      "\n")
     nil
     pkg-file
     nil nil nil 'excl)))

(defun package-build-get-config (file-name)
  "get the configuration information for the given file-name"
  (save-window-excursion
    (let ((config-file (format "~/.emacs.d/elisp/epkgs/%s/.config" file-name)))
      (cond
       ((file-exists-p config-file)
        (find-file config-file)
        (car
         (read-from-string
          (buffer-substring-no-properties (point-min) (point-max)))))
       (t nil)))))

(defun package-build-create-tar (file-name version)
  "create a tar for the file-name with version"
  (let ((default-directory package-build-working-dir)
        (base-dir (concat file-name "-" version)))
    (process-file
     "tar" nil
     (get-buffer-create "*package-build-checkout*")
     nil "-cvf"
     (expand-file-name
      (concat base-dir ".tar")
      package-build-archive-dir)
     "--exclude=.svn"
     "--exclude=.git*"
     base-dir)))



(defun package-build-archive (file-name)
  "this is the first attempt at building a package strictly for yasnippet"
  (interactive)
  (let* ((desc (package-build-get-config file-name))
         (version (format-time-string "%Y%m%d" (current-time)))
         (base-dir (concat file-name "-" version))
         (dir (expand-file-name base-dir package-build-working-dir))
         (pkg-file (expand-file-name (concat file-name "-pkg.el") dir)))
    (message base-dir)
    (when desc
      (let* ((repo-type (plist-get desc :fetcher))
             (repo (plist-get desc :url))
             (homepage (plist-get desc :homepage)))
        (cond
         ((eq repo-type 'svn)
          (message "SVN TYPE")
          (package-build-checkout-svn repo dir))
         ((eq repo-type 'git)
          (message "GIT TYPE")
          (package-build-checkout-git repo dir)))

        (when (file-exists-p dir)
          (unless (file-exists-p pkg-file)
            (package-build-pkg-file pkg-file file-name version homepage))
          (package-build-create-tar file-name version)
          )))))
  
