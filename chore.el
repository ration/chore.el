;;; chore.el --- Bootstrap chores  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Tatu Lahtela

;; Author:     Tatu Lahtela <lahtela@iki.fi>
;; Maintainer: Tatu Lahtela <lahtela@iki.fi>
;; Version:    0.1.3
;; Keywords:   lisp, clubhouse, git
;; Homepage:   https://github.com/doublep/datetime
;; Package-Requires: ((emacs "24.4") (magit))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.


;;; Commentary:

;; Helpers for organizing creation and handling of chores.
;; Creates the repeatable infrastructure for a new chore, such as
;; a new org file for notes and a git branch.

;; See also https://github.com/arbox/org-sync

;;; Code:

;;; Customization
(defcustom chore-current-project-subdir "" "Subdiretory for notes files." :type '(string) :group 'chore)
(defcustom chore-current-project-git-root nil "GIT root for current project." :type '(string) :group 'chore)
(defcustom chore-single-note-file "" "If non nill this file as the org file." :type '(string) :group 'chore)

(defcustom chore-backend "forge" "Backend for chores." :type '(string) :group 'chore)

(let ((backend-file (expand-file-name (concat "chore-" chore-backend ".el") (file-name-directory load-file-name))))
  (if (file-exists-p backend-file) (load-file backend-file)))

(defcustom chore-notes-root  (concat (getenv "HOME") "/Org/" chore-current-project-subdir)
  "Root directory for notes files.
Creates a note file in this directory unless CHORE--SINGLE-NOTE-FILE is set." :type '(string) :group 'chore)

;; TODO allow changing current note
(defcustom chore-current-note nil "Current note location." :type '(string) :group 'chore)


;;; Backend multimethods
(defun chore-apply (suffix &rest args)
  "Simple facility to emulate multimethods.
Apply SUFFIX to spotify-prefixed functions, applying ARGS."
  (let ((func-name (format "chore-%s-%s" chore-backend suffix)))
    (apply (intern func-name) args)))

(defun chore--get-chores ()
  "Retrieve list of chores from backend."
  (chore-apply "get-chores"))

(defun chore--create-org-entry (chore)
  "Create org entry for CHORE."
  (chore-apply "create-org-entry" chore))

(defun chore--branch-name (chore)
  "Branch name to create for CHORE."
  ;; TODO as config
  (chore-apply "branch-name" chore))

;;; Common methods

(defun chore-switch-to-note ()
  "Switch to note file if set."
  (interactive)
  (let ((chore-note-file (if chore-single-note-file chore-single-note-file chore-current-note)))
    (if chore-note-file
        (if (string-equal (file-truename buffer-file-name) (file-truename chore-note-file))
            (switch-to-buffer (other-buffer (current-buffer)))
          (find-file chore-note-file)))))

(defun chore--chore-name-cleaned (chore)
  "Cleanup CHORE name into something sensible for the file name."
  (replace-regexp-in-string "[ :]" "_" (cdr chore)))

(defun chore--create-org-file (chore)
  "Create notes file for the CHORE."
  (interactive)
  ;; TODO customize ch from backend
  (let ((notes-file (concat chore-notes-root (format "/ch%s-%s" (car chore) (chore--chore-name-cleaned chore))
                            ".org")))
    (setq chore-current-note notes-file)
    (find-file-other-window notes-file)
    (chore--create-org-entry chore)
    (save-buffer)))

(defun chore--create-org-headline (chore)
  "Create chore note to end in existing org file."
  (find-file chore-single-note-file)
  (goto-char (point-max))
  (chore--create-org-entry chore))

(defun chore--check-clean-repo ()
  "Check or verify that there aren't anything important ongoing."
  (if (or (magit-unstaged-files) (magit-staged-files))
      (if (y-or-n-p "There are unstaged files.  Continue anyway? " )
           t
        nil)
    t))

(defun chore--prompt-branch-name (chore)
  "Prompt user for new branch name for CHORE."
(interactive)
    (read-string
     (format "New branch name: (%s): " (chore--branch-name chore)) ; prompt. It's nice to show the default value
     (chore--branch-name chore) ; initial input.  This value is prefilled in the mini-buffer. Available but considered deprecated.
     nil ; history list if you have a specific one for this
     (chore--branch-name chore) ; Whatever this evaluates to will be the default value
     ))

(defun chore--create-branch (chore)
  "Create branch for CHORE."
  (if chore-current-project-git-root
      (if (y-or-n-p "Pull master and create branch? ")
          (progn
            (with-temp-buffer
              (cd chore-current-project-git-root)
              (if (and (y-or-n-p "Create branch for this chore? ") (chore--check-clean-repo))
                  (magit-branch-and-checkout (chore--prompt-branch-name chore) (completing-read "Starting point: "(magit-list-remote-branch-names))))))
        (message "Git root not set, doing nothing"))))

(defun find--alist-entry (target alist)
  "Find TARGET entry in alist ALIST."
  (car (seq-filter (lambda (k) (string-equal (cdr k) target)) alist)))

(defun chore-pick-chore ()
  (interactive)
  (let ((chores (chore--get-chores)))
    (find--alist-entry
     (completing-read "Select Story: " (-map #'cdr chores)) chores)))

(defun chore-new-chore ()
  "Execute this when selecting a new chore."
  (interactive)
  (let ((chore (chore-pick-chore)))
    (chore--create-branch chore)
    (if chore-single-note-file
        (chore--create-org-headline chore)
      (chore--create-org-file chore))))

(provide 'chore)

;;; chore.el ends here
