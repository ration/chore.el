;; Forge backend for chore
;; Package-Requires: ((emacs "24.4") (forge)
;;
;; If your issue backend is the same as in forge

;;; Code:

(defun chore-forge-get-chores ()
  "Get cons list of task id - title from Forge."
  (with-temp-buffer
    (cd chore-current-project-git-root)
    (mapcar (lambda (x) (cons (car x) (car (last x)))) (forge-sql [:select $i1 :from issue :where (= repository $s2)]
                                                                  [number title]
                                                                  (oref (forge-get-repository t) id)))))
(defun chore-forge-create-org-entry (chore)
  "Create ORG entry for CHORE."
  (save-mark-and-excursion
    (insert (format
     "* %s: %s
:PROPERTIES:
:github-issue: %s
:END:

"
 (car chore) (cdr chore) (car chore)))))

(defun chore-forge-branch-name (chore)
  "Branch name for CHORE."
  (format "issues/%s/%s" (car chore) (replace-regexp-in-string "[ :]" "-" (cdr chore))))

(provide 'chore-forge)

;;; chore-forge.el ends here
