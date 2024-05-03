;; Azure devops backend for chode

;; Requires chore-azure-devops-token to be set


;; To make use of this, you need to have a query stored in azure devops that returns a list of tickets
;; relevant to you


(defcustom chore-azure-devops-token nil "Login token for azure devops" :type (string) :group 'chore)
(defcustom chore-azure-devops-url "https://dev.azure.com/<organization>/<project>/_apis/"
  "Standard query for devops queries. You need to set the chore-azure-devops-organization and chore-azure-devops-project.
" :type (string) :group 'chore)
(defcustom chore-azure-devops-organization nil "Organization in Azure devops")
(defcustom chore-azure-devops-project nil "Project in Azure devops")
(defcustom chore-azure-devops-ticket-query-id nil "Custom query to fetch work items. You need to create this yourself inside azure devops"
  :type (string) :group 'chore)

(defcustom chore-azure-devops-api-version "api-version=6.0" "Used API version")

(defun chore-azure-devops-headers ()
  `(("Accept" . "application/json")
               ("Authorization" . ,(format "Basic %s" (base64-encode-string (concat ":" chore-azure-devops-token))))))

(defun chore-azure-get-work-item-urls ()
    (let-alist (plz 'get (format "%swit/wiql/%s?%s" (chore-azure-devops-get-url) chore-azure-devops-ticket-query-id chore-azure-devops-api-version)
    :headers (chore-azure-devops-headers)
    :as #'json-read
    )
      (mapcar (lambda (x) (cdr (assoc 'url x))) (let-alist azz .workItems))
    ))

(defun chore-azure-devops-get-ticket (url)
  (plz 'get url :headers (chore-azure-devops-headers) :as #'json-read))

(defun chore-azure-devops-get-chores ()
  "Get cons list of task id - title from Azure DevOps"
  (mapcar
   (lambda (item) (let-alist item
                    (cons .id (cdr (assoc 'System.Title .fields)))))
   (mapcar (lambda (url) (chore-azure-devops-get-ticket url)) (chore-azure-get-work-item-urls))
   ))


(defun chore-azure-devops-create-org-entry ())

(defun chore-azure-devops-get-url ()
  (string-replace "<project>" chore-azure-devops-project
                  (string-replace "<organization>" chore-azure-devops-organization chore-azure-devops-url)))

(provide 'chore-azure-devops)

