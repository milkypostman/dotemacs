;;; package-maint.el --- Tools for curating the package archive

;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 2 Jan 2009
;; Version: 0.9
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

(defvar package-index "~/src/package.el/index.el"
  "The listing of all projects and repositories to get them from.
Should contain an list of projects, each formatted as a list with
the project name followed by the DVCS repository URL.")

(defvar package-working-dir "~/src/package.el/working/%s"
  "Directory in which to keep project checkouts.")

(defvar package-public-dir "~/src/package.el/public"
  "Directory in which to place packages created.")

(defvar package-version-format "^\\([0-9\\.]+[0-9]\\)*$"
  "A regex that will only match tags which indicate versions.")

(defun package-build-archive ()
  "Build packages for every version of every project in the index."
  (interactive)
  (save-window-excursion
    (find-file package-index)
    (let ((original-dir default-directory)
          (projects (package-read-from-string
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))
      (dolist (project projects)
        (package-build-packages project))
      (package-build-archive-contents projects)
      (cd original-dir))))

(defun package-build-packages (project)
  "Given a project, create packages for each version needs building."
  (let ((name (car project)))
    (package-init project)
    (cd (package-local-checkout-dir name))
    (shell-command "git fetch --tags")
    (dolist (version (package-list-versions))
      (when (not (package-built? name version))
        (package-build-package name version)))))

(defun package-build-package (name version)
  "Given a project version, create a package for it."
  (message "Building %s v%s" name version)
  (let ((package-source (format "%s/%s.el"
                                (package-local-checkout-dir name) name)))
    (cd (package-local-checkout-dir name))
    (shell-command (format "git checkout %s" version))
    (if (not (file-exists-p package-source))
        (message "Skipping %s since %s was not found." name package-source)
      (find-file package-source)
      (package-write-buffer)
      (message "Built %s version %s." name version)
      (kill-buffer))))

(defun package-write-buffer ()
  "Write a package whose contents are in the current buffer."
  (save-excursion
    (save-restriction
      (let* ((pkg-info (package-buffer-info))
             (pkg-version (aref pkg-info 3))
             (file-name (aref pkg-info 0)))
        (make-directory package-public-dir t)
        (write-region (point-min) (point-max)
                      (concat package-public-dir "/"
                              file-name "-" pkg-version ".el")
                      nil nil nil)
        ;; special-case "package": write a second copy so that the
        ;; installer can easily find the latest version.
        (if (string= file-name "package")
            (write-region (point-min) (point-max)
                          (concat package-public-dir "/"
                                  file-name ".el")
                          nil nil nil 'ask))))))

(defun package-init (project)
  "Create a new checkout of a project if necessary."
  (when (not (file-exists-p (package-local-checkout-dir (car project))))
    (make-directory package-working-dir t)
    (cd (format package-working-dir ""))
    (shell-command (format "git clone %s %s" (cadr project) (car project)))))

(defun package-local-checkout-dir (name)
  (format package-working-dir name))

(defun package-list-versions ()
  "List all versions of a project. Must run in project checkout."
  (remove-if-not (lambda (v) (string-match package-version-format v))
                 (split-string (shell-command-to-string "git tag")
                               "\n" t)))

(defun package-public-file (name version)
  (format "%s/%s-%s.el" package-public-dir name version))

(defun package-built? (name version)
  (file-exists-p (package-public-file name version)))

(defun package-build-archive-contents (projects)
  "Update the list of packages."
  (let ((print-level nil)
        (print-length nil)
        (contents (package-get-archive-contents projects)))
    (write-region (concat (pp-to-string contents) "\n") nil
                  (concat package-public-dir
                          "/archive-contents"))))

(defun package-get-archive-contents (projects)
  (cons package-archive-version
        (remove-if
         'null (mapcar 'package-archive-contents-for-project projects))))

(defun package-archive-contents-for-project (project)
  (when (file-exists-p (package-latest-for-project project))
    (find-file (package-latest-for-project project))
    (let* ((pkg-info (package-buffer-info))
           (pkg-version (aref pkg-info 3))
           (split-version (package-version-split pkg-version))
           (requires (aref pkg-info 1))
           (desc (if (string= (aref pkg-info 2) "")
                     (read-string "Description of package: ")
                   (aref pkg-info 2)))
           ;; TODO: support tar
           (file-type 'single))
      (cons (intern (car project))
            (vector split-version requires desc file-type)))))

(defun package-latest-for-project (project)
  (cd (package-local-checkout-dir (car project)))
  (let* ((name (car project))
         (versions (package-list-versions))
         (latest-version (car (last (package-sort-versions versions)))))
    (package-public-file name latest-version)))

(defun package-sort-versions (versions)
  ;; destructive list functions! you gotta be kidding me.
  (let ((versions (copy-list versions)))
    (sort versions (lambda (a b)
                     (package-version-compare
                      (package-version-split a)
                      (package-version-split b) '<)))))

(provide 'package-maint)
;;; package-maint.el ends here
