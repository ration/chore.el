;;; task-organizer.el --- Bootstrap tasks  -*- lexical-binding: t -*-

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

(defcustom task-oragnizer--current-project-git-root nil "GIT root for current project")

(defcustom task-organizer--notes-root  (concat "/home/lahtela/Org/" current-project) "Root directory for notes files")

;; Requires org-clubhouse
(defun task-organizer--get-clubhouse-tasks ()
  "Get cons list of task id - title from Clubhouse"
  (to-id-name-pairs (org-clubhouse--search-stories
                          (format "owner:%s !is:done !is:archived"
                                  org-clubhouse-username))))

(defun task-organizer--task-name-cleaned (task)
  (replace-regexp-in-string "[ :]" "_" (cdr task)))

(defun task-organizer--branch-name-cleaned (task)
  (format "feature/ch%s/%s" (car task) (replace-regexp-in-string "[ :]" "-" (cdr task))))

(defun task-organizer--create-org-file (task)
  "Create notes file for the new task.

   Creates the initial level with org-clubhouse"
  (interactive)
  ;;(dired-create-empty-file (concat task--organizer-notes-root "/" (format) ))
  (let ((notes-file (concat task-organizer--notes-root (format "/ch%s-%s" (car task) (task-organizer--task-name-cleaned task))
                            ".org")))
    (find-file-other-window notes-file)
    (org-clubhouse-headline-from-story-id 1 (car task))
    (save-buffer)))

(defun task-organizer--check-clean-repo ()
  "Check or verify that there aren't anything important ongoing"
  (if (or (magit-unstaged-files) (magit-staged-files))
      (if (y-or-n-p "There are unstaged files. Continue anyway?" )
           t
        nil)
    t))

(defun task-organizer--prompt-branch-name (task)
  "Prompt user for new branch name"
(interactive)
    (read-string
     (format "New branch name: (%s): " (task-organizer--branch-name-cleaned task)) ; prompt. It's nice to show the default value
     (task-organizer--branch-name-cleaned task) ; initial input.  This value is prefilled in the mini-buffer. Available but considered deprecated.
     nil ; history list if you have a specific one for this
     (task-organizer--branch-name-cleaned task) ; Whatever this evaluates to will be the default value
     ))

(defun task-organizer--create-branch (task)
  (interactive)
  (if task-oragnizer--current-project-git-root
      (if (y-or-n-p "Pull master and create branch? ")
          (progn
            ;; TODO x
            (with-temp-buffer
              (cd task-oragnizer--current-project-git-root)
              (if (and (y-or-n-p "Create branch for this task? ") (task-organizer--check-clean-repo))
                  (magit-branch-and-checkout (task-organizer--prompt-branch-name task) (completing-read "Starting point: "(magit-list-remote-branch-names))))))
        (message "Git root not set, doing nothing"))))

(defun find--alist-entry (target alist)
  (->> alist
       (-find (lambda (key-value)
                   (string-equal (cdr key-value) target)))))

(defun task-organizer-new-task ()
  "Execute this when selecting a new task"
  (interactive)
  (let* ((tasks (task-organizer--get-clubhouse-tasks))
         (task (find--alist-entry
          (completing-read "Select Story: "
                           (-map #'cdr tasks)) tasks)))
    (task-organizer--create-branch task)
    (task-organizer--create-org-file task)))

;;(task-organizer-new-task)


