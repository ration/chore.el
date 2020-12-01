;; Clubhouse backend for chore

;; Requires org-clubhouse
(defun chore--get-tasks ()
  "Get cons list of task id - title from Clubhouse"
  (to-id-name-pairs (org-clubhouse--search-stories
                          (format "owner:%s !is:done !is:archived"
                                  org-clubhouse-username))))

(provide 'chore-clubhouse)
