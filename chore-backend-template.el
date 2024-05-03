;; Template backend for chore
;; To implement a new backend implement all methods
;; and replace all BACKEND with name

;; Requires org-clubhouse
(defun chore-BACKEND-get-chores ()
  "Get cons list of task id - title from Clubhouse"
  (to-id-name-pairs (org-clubhouse--search-stories
                          (format "owner:%s !is:done !is:archived"
                                  org-clubhouse-username))))

(defun chore-BACKEND-create-org-entry ())

(provide 'chore-BACKEND)
