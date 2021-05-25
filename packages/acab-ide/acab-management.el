;; -*- mode: elisp; lexical-binding: t; -*-
;;
;;Variables:
(defvar trie/dialects '()
  "All defined sub-dsls")
(defvar trie/current-priors '()
  "Available Priors for the current rule")
(defvar trie/current-post '()
  "Available Posts for the current rule")

;;Registered databases for lookup and tracking
(defvar trie/rules nil
  "Defined rules")
(defvar trie/types nil
  "Loaded and Defined types")
(defvar trie/crosscuts nil
  "Loaded and defined crosscuts")
(defvar trie/patterns nil
  "Loaded and defined patterns")
(defvar trie/tests nil
  "Defined tests")

(defvar trie/sentences nil)
(defvar trie/tags nil
  "Tag references to other objects")
(defvar trie/channels nil
  "Specified channels between layers")

(defun trie/init-data ()
  (setq trie/rules (make-hash-table :test 'equal)
        trie/types (make-hash-table :test 'equal)
        trie/crosscuts (make-hash-table :test 'equal)
        trie/patterns (make-hash-table :test 'equal)
        trie/tests (make-hash-table :test 'equal)
        trie/tags (make-hash-table :test 'equal)
        trie/channels (make-hash-table :test 'equal)

        trie/current-priors '()
        trie/current-post '()
        )
  )

;;Helm
(defun trie/get-rule-helm-candidates ()
  "Generate candidates for rule helm"
  (hash-table-keys trie/rules)
  )
(defun trie/find-rule (x)
  (message "Get rules: %s" (helm-marked-candidates))
  ;;TODO find rules
  ;;Retrieve rules
  ;;update working ruleset
  ;;open rule files
  (let ((buf (if (get-buffer x) x (find-file x)))
        (window (if (window-valid-p (plist-get acab-ide/window-configuration :rule))
                    (plist-get acab-ide/window-configuration :rule)
                  (selected-window)))
        )
    (window--display-buffer buf window 'window)
    )
  )
(defun trie/create-rule (x)
  (message "Creating Rule: %s" x)
  ;;TODO create rule
  ;;update working ruleset
  ;;open rule file
  ;;insert template
  ;;update rule hashtable
  (let* ((trie-snippet-rule-name x)
         (file-string (format "%s.rule" (s-replace "." "_" x)))
         (rule-path (f-join acab-ide/ide-data-loc "rules" file-string))
         (window (if (window-valid-p (plist-get acab-ide/window-configuration :rule))
                     (plist-get acab-ide/window-configuration :rule)
                   (selected-window)))
         )

    (window--display-buffer (generate-new-buffer x) window 'window)
    (with-current-buffer x
      (trie-mode)
      (yas-expand-snippet (yas-lookup-snippet "rule")
                          (point-min) (point-max))
      (write-file rule-path)
      (puthash x rule-path trie/rules)
      )
    )
  )

(defun trie/get-type-helm-candidates ()
  (hash-table-keys trie/types)
)
(defun trie/find-type (x) )
(defun trie/create-type (x) )

(defun trie/get-pattern-helm-candidates ()
  (hash-table-keys trie/patterns)
  )
(defun trie/find-pattern (x) )
(defun trie/create-pattern (x) )

(defun trie/get-test-helm-candidates ()
  (hash-table-keys trie/tests)
)
(defun trie/find-test (x) )
(defun trie/create-test (x) )

(defun trie/get-sentence-helm-candidates ()
  trie/sentences
)
(defun trie/add-sentence () )
(defun trie/add-new-sentence () )

(defun trie/get-crosscut-helm-candidates ()
  (hash-table-keys trie/crosscuts)
)
(defun trie/add-crosscut ())

(defun trie/get-tag-helm-candidates ()
  trie/tags
)
(defun trie/toggle-tag (x)
  ;;find tags
  ;;remove selected from existing
  ;;add remaining
)
(defun trie/create-tag(x)
  ;;find tags
  ;;add selected
)


(defun trie/get-channel-helm-candidates ()
  trie/channels
)
(defun trie/create-channel () )
(defun trie/find-channel ())

;;DELETION
(defun trie/delete-rule ()
  (interactive)
  ;;TODO delete rule
  ;;remove from hashmap
  ;;delete file
  ;;remove from runtime
  )
(defun trie/delete-type ()
  (interactive)
  ;;TODO delete type
  )
(defun trie/delete-crosscut ()
  (interactive)
  ;;TODO delete crosscut
  )
(defun trie/delete-sequence ()
  (interactive)
  ;;TODO delete sequence
  )

;;VISUAL
(defun trie/decrement-priors-layer ()
  (interactive)
  ;;TODO decrement priors layer
  )
(defun trie/increment-priors-layer ()
  (interactive)
  ;;TODO increment priors layer
  )
(defun trie/decrement-posts-layer ()
  (interactive)
  ;;TODO decrement post layer
  )
(defun trie/increment-posts-layer ()
  (interactive)
  ;;TODO increment post layer
  )

(defun trie/show-analysis-results ()
  ;;TODO show analysis results
  )
(defun trie/show-side-buffer ()
  ;;TODO show side buffer
  )
(defun trie/write-io-info-buffer (data target)
  "Insert new data into IO buffers"
  (with-current-buffer (cond ((eq :input target)
                              trie/inputs-buffer-name)
                             ((eq :output target)
                              trie/outputs-buffer-name)
                             (t nil))
    ;;Clear
    (jg_layer/clear-buffer)
    ;;Insert header + layer
    (insert (format "* Available %s:\n" (cond ((eq :input target)
                                               "Inputs")
                                              ((eq :output target)
                                               "Outputs"))))

    ;;Insert data strings
    (loop for x in (plist-get data :list) do
          (insert (format "  %s\n" x)) ;;Maybe propertize
          )
    )
  )

;;UPDATE
(defun trie/update-buffer-contents ()
  (interactive)
  ;;TODO update buffer contents
  )
(defun trie/sort-conditions-and-actions ()
  ;;TODO sort conditions and actions
  )
;;INSERTION
(defun trie/el-string-helm ()
  (interactive)
  ;;TODO el string helm
  )
(defun trie/insert-tag ()
  (interactive)
  ;;TODO insert tag
  ;;select rule window
  ;;find tags
  ;;populate tags
  ;;run tag helm

  )
(defun trie/insert-transform ()
  ;;TODO insert transform
  ;;select rule window
  ;;find tags
  ;;populate tags
  ;;run transform helm

  )
(defun trie/insert-action ()
  ;;TODO insert action
  ;;select rule window
  ;;find tags
  ;;populate tags
  ;;run action helm
  )
(defun trie/insert-from-side-buffer ()
  ;;TODO insert from side buffer
  )

;;REMOVAL
(defun trie/remove-component ()
  ;;TODO remove component
  )

;;CLEANUP
(defun trie/cleanup ()
  ;;TODO cleanup
  )
(defun trie/write-rules-to-files ()
  ;;TODO write rules to files
  )
;;Analysis
(defun trie/analyse-data ()
  ;;TODO analyse data
  )

(provide 'trie-management)
