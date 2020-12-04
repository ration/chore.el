;; Github backend for chore
(defun chore-github-get-chores ()
  "Get cons list of task id - title from Clubhouse"
  (to-id-name-pairs (org-clubhouse--search-stories
                          (format "owner:%s !is:done !is:archived"
                                  org-clubhouse-username))))

(defun chore-github-create-org-entry ()
  "")

(provide 'chore-github)
