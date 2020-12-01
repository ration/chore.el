;;; chore.el --- Bootstrap tasks  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Tatu Lahtela

;; Author:     Tatu Lahtela <lahtela@iki.fi>
;; Maintainer: Tatu Lahtela <lahtela@iki.fi>
;; Version:    0.1.0
;; Keywords:   lisp, clubhouse, git
;; Homepage:   https://github.com/doublep/datetime
;; Package-Requires: ((emacs "24.4") (org-clubhouse) (magit))

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

;; Helpers for organizing creation and handling of tasks.
;; Creates the repeatable infrastructure for a new task, such as
;; a new org file for notes and a git branch.

;;; Code:

;; Set current-project if you want a subdirectory for notes for the
;; current project
(unless (boundp 'current-project) (setq current-project ""))

(defcustom chore-current-project-git-root nil "GIT root for current project")

;; TODO other task backends
(defcustom chore-ticket-backend "clubhouse" "Backend for tasks")
(require 'chore-clubhouse)

;; TODO
(defcustom chore-single-note-file nil "If non nill this file as the org file")

(defcustom chore-notes-root  (concat (getenv "HOME") "/Org/" current-project)
  "Root directory for notes files. Creates a note file in this directory unless CHORE--SINGLE-NOTE-FILE is set")

;; TODO allow changing current note
(defcustom chore-current-note nil "Current note location")

(defun chore-switch-to-note ()
  "Switch to note file if set"
  (interactive)
  (if (string-equal (buffer-file-name) chore-current-note)
      (switch-to-buffer (other-buffer (current-buffer) 1))
    (if chore-current-note
        (find-file chore-current-note))))

(defun chore--task-name-cleaned (task)
  (replace-regexp-in-string "[ :]" "_" (cdr task)))

(defun chore--branch-name-cleaned (task)
  ;; TODO as config
  (format "feature/ch%s/%s" (car task) (replace-regexp-in-string "[ :]" "-" (cdr task))))

(defun chore--create-org-file (task)
  "Create notes file for the new task.

   Creates the initial level with org-clubhouse"
  (interactive)
  ;; TODO customize ch
  (let ((notes-file (concat chore--notes-root (format "/ch%s-%s" (car task) (chore--task-name-cleaned task))
                            ".org")))
    (setq chore-current-note notes-file)
    (find-file-other-window notes-file)
    (org-clubhouse-headline-from-story-id 1 (car task))
    (save-buffer)))

(defun chore--check-clean-repo ()
  "Check or verify that there aren't anything important ongoing"
  (if (or (magit-unstaged-files) (magit-staged-files))
      (if (y-or-n-p "There are unstaged files. Continue anyway?" )
           t
        nil)
    t))

(defun chore--prompt-branch-name (task)
  "Prompt user for new branch name"
(interactive)
    (read-string
     (format "New branch name: (%s): " (chore--branch-name-cleaned task)) ; prompt. It's nice to show the default value
     (chore--branch-name-cleaned task) ; initial input.  This value is prefilled in the mini-buffer. Available but considered deprecated.
     nil ; history list if you have a specific one for this
     (chore--branch-name-cleaned task) ; Whatever this evaluates to will be the default value
     ))

(defun chore--create-branch (task)
  (interactive)
  (if chore-current-project-git-root
      (if (y-or-n-p "Pull master and create branch? ")
          (progn
            ;; TODO x
            (with-temp-buffer
              (cd chore-current-project-git-root)
              (if (and (y-or-n-p "Create branch for this task? ") (chore--check-clean-repo))
                  (magit-branch-and-checkout (chore--prompt-branch-name task) (completing-read "Starting point: "(magit-list-remote-branch-names))))))
        (message "Git root not set, doing nothing"))))

(defun find--alist-entry (target alist)
  (->> alist
       (-find (lambda (key-value)
                   (string-equal (cdr key-value) target)))))

(defun chore-new-task ()
  "Execute this when selecting a new task"
  (interactive)
  (let* ((tasks (chore--get-tasks))
         (task (find--alist-entry
          (completing-read "Select Story: "
                           (-map #'cdr tasks)) tasks)))
    (chore--create-branch task)
    (chore--create-org-file task)))

;;(chore-new-task)

(provide 'chore)

