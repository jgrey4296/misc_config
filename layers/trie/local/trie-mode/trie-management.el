;;Variables:
(defvar trie/python-process nil)
(defvar trie/dialects '()
  "All defined sub-dsls")
(defvar trie/cwd nil
  "The Working directory of Trie Mode")
(defvar trie/current-priors '()
  "Available Priors for the current rule")
(defvar trie/current-post '()
  "Available Posts for the current rule")

;;Registered databases for lookup and tracking
(defvar trie/rules '()
  "Defined rules")
(defvar trie/types '()
  "Loaded and Defined types")
(defvar trie/crosscuts '()
  "Loaded and defined crosscuts")
(defvar trie/patterns '()
  "Loaded and defined patterns")
(defvar trie/tests '()
  "Defined tests")

(defvar trie/tags '()
  "Tag references to other objects")
(defvar trie/channels '()
  "Specified channels between layers")

;;Functions
;;CREATION
(defun trie/find-or-create-rule ()
  (interactive)
  ;; helm
  )
(defun trie/find-or-create-type ()
  (interactive)
)
(defun trie/find-or-create-crosscut ()
  (interactive)
  )
(defun trie/find-or-create-sequence ()
  (interactive)
  )

;;Helm
(defun trie/get-rule-helm-candidates ()
  "Generate candidates for rule helm"
  (hash-table-keys trie/rules)
  )
(defun trie/find-rule (x)
  (message "Get rules: %s" (helm-marked-candidates))
  ;;Retrieve rules
  ;;update working ruleset
  ;;open rule files
  )
(defun trie/create-rule (x)
  (message "Creating Rule: %s" x)
  ;;update working ruleset
  ;;open rule file
  ;;insert template
  ;;update rule hashtable
  )
;;DELETION
(defun trie/delete-rule ()
  (interactive)
  )
(defun trie/delete-type ()
  (interactive)
  )
(defun trie/delete-crosscut ()
  (interactive)
  )
(defun trie/delete-sequence ()
  (interactive)
  )

;;VISUAL
(defun trie/decrement-buffer-content-layer ()
  (interactive)
  )
(defun trie/increment-buffer-content-layer ()
  (interactive)
  )
(defun trie/show-analysis-results ())
(defun trie/show-side-buffer ())
;;UPDATE
(defun trie/update-buffer-contents ()
  (interactive)
  )
(defun trie/sort-conditions-and-actions ())
;;INSERTION
(defun trie/el-string-helm ()
  (interactive)
  )
(defun trie/insert-tag ()
  (interactive)
  )
(defun trie/insert-transform ())
(defun trie/insert-action ())
(defun trie/insert-from-side-buffer ())

;;REMOVAL
(defun trie/remove-component ())

;;CLEANUP
(defun trie/cleanup ())
(defun trie/write-rules-to-files ())
;;Analysis
(defun trie/analyse-data ())


