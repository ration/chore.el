;; Clubhouse backend for chore

;; (defvar org-clubhouse-username)

;; Requires org-clubhouse
(defun chore-clubhouse-get-chores ()
  "Get cons list of task id - title from Clubhouse"
  (to-id-name-pairs (org-clubhouse--search-stories
                          (format "owner:%s !is:done !is:archived"
                                  org-clubhouse-username))))

(defun chore-clubhouse-create-org-entry (chore)
  "Create ORG entry for chore."
  (org-clubhouse-headline-from-story-id 1 (car chore)))

(defun chore-clubhouse-branch-name (chore)
  (format "feature/ch%s/%s" (car chore) (replace-regexp-in-string "[ :]" "-" (cdr chore))))

(provide 'chore-clubhouse)
