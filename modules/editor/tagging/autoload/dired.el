;;; +tags.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-tag-dired-describe-marked-tags ()
  "Describe tags in marked files"
  (interactive)
  (let ((marked (dired-get-marked-files))
        (targetdepth (or current-prefix-arg 2))
        (alltags (make-hash-table :test 'equal))
        )
    ;; (message "Describing marked file tags to depth: %s" targetdepth)
    (cl-loop for x in marked do
          (maphash (lambda (k v) (cl-incf (gethash k alltags 0) v)) (tagging-minor-mode/org-get-file-tags x targetdepth))
          )
    (if (not (hash-table-empty-p alltags))
        (librarian-tagging-chart-chart-tag-counts alltags "Dired Marked Files")
      (message "No Tags in Files")
      )
    )
  )

;;;###autoload
(defun +jg-tag-dired-mark-untagged()
  "Mark org files which are not tagged at heading depth 2"
  (interactive)
  (dired-map-over-marks
   (progn (if (or (not (f-ext? (dired-get-filename) "org"))
                  (tagging-minor-mode/org-tagged-p (dired-get-filename)))
              (dired-unmark 1)))
   nil
   )
  )

;;;###autoload
(defun +jg-tag-dired-count-untagged ()
  "Count marked org files that are untagged"
  (interactive)
  (let ((counts 0)
        (untagged-p (lambda (x) (not (tagging-minor-mode/org-tagged-p x))))
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
