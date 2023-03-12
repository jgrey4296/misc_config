;; dired actions

(defun +jg-tag-describe-marked-tags ()
  "Describe tags in marked files"
  (interactive)
  (let ((marked (dired-get-marked-files))
        (targetdepth (or current-prefix-arg 2))
        (alltags (make-hash-table :test 'equal))
        )
    ;; (message "Describing marked file tags to depth: %s" targetdepth)
    (cl-loop for x in marked do
          (maphash (lambda (k v) (cl-incf (gethash k alltags 0) v)) (+jg-tag-get-file-tags x targetdepth))
          )
    (if (not (hash-table-empty-p alltags))
        (+jg-tag-chart-tag-counts alltags "Dired Marked Files")
      (message "No Tags in Files")
      )
    )
  )
(defun +jg-tag-mark-untagged-orgs ()
  "Mark org files which are not tagged at heading depth 2"
  (interactive)
  (dired-map-over-marks
   (progn (if (or (not (f-ext? (dired-get-filename) "org"))
                  (+jg-tag-org-tagged-p (dired-get-filename)))
              (dired-unmark 1)))
   nil
   )
  )
(defun +jg-tag-dired-directory-count-untagged ()
  "Count marked org files that are untagged"
  (interactive)
  (let ((counts 0)
        (untagged-p (lambda (x) (not (+jg-tag-org-tagged-p x))))
        )
    (dired-map-over-marks
     (if (f-dir? (dired-get-filename))
         (cl-incf counts (length
                       (seq-filter untagged-p (directory-files-recursively (dired-get-filename) "\.org"))
                       )
               )
       )
     nil
     )
    (message "%s Org files untagged" counts)
    )
  )
