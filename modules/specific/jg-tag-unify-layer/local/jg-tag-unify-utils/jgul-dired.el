;; dired actions
(provide 'jgul-dired)

(defun jg-tag-unify-layer/describe-marked-tags ()
  "Describe tags in marked files"
  (interactive)
  (let ((marked (dired-get-marked-files))
        (targetdepth (or current-prefix-arg 2))
        (alltags (make-hash-table :test 'equal))
        )
    ;; (message "Describing marked file tags to depth: %s" targetdepth)
    (loop for x in marked do
          (maphash (lambda (k v) (incf (gethash k alltags 0) v)) (jg-tag-unify-layer/get-file-tags x targetdepth))
          )
    (if (not (hash-table-empty-p alltags))
        (jg-tag-unify-layer/chart-tag-counts alltags "Dired Marked Files")
      (message "No Tags in Files")
      )
    )
  )
(defun jg-tag-unify-layer/mark-untagged-orgs ()
  "Mark org files which are not tagged at heading depth 2"
  (interactive)
  (dired-map-over-marks
   (progn (if (or (not (f-ext? (dired-get-filename) "org"))
                  (jg-tag-unify-layer/org-tagged-p (dired-get-filename)))
              (dired-unmark 1)))
   nil
   )
  )
(defun jg-tag-unify-layer/dired-directory-count-untagged ()
  "Count marked org files that are untagged"
  (interactive)
  (let ((counts 0)
        (untagged-p (lambda (x) (not (jg-tag-unify-layer/org-tagged-p x))))
        )
    (dired-map-over-marks
     (if (f-dir? (dired-get-filename))
         (incf counts (length
                       (seq-filter untagged-p (directory-files-recursively (dired-get-filename) "\.org"))
                       )
               )
       )
     nil
     )
    (message "%s Org files untagged" counts)
    )
  )
(defun jg-tag-unify-layer/find-random-marked-file ()
  "Open random file from marked"
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (find-file (nth (random (length marked))
                    marked))
    )
  )
(defun jg-tag-unify-layer/quick-compress-orgs ()
  "Find all orgs in cwd down, compress together"
  (interactive)
  (let* ((curr default-directory)
         (files (directory-files-recursively curr "\\.org"))
         (target_dir "compressed_orgs")
         )
    ;;Make the top level
    (if (not (f-exists? (f-join curr target_dir)))
        (mkdir (f-join curr target_dir)))
    ;;Copy files over
    (mapc (lambda (x)
            (let ((target (f-join curr target_dir (-last-item (f-split (f-parent x))))))
              (if (not (f-exists? target)) (mkdir target))
              (copy-file x (format "%s/" target))
              )
            ) files)
    (dired-compress-file (f-join curr target_dir))
    (delete-directory (f-join curr target_dir) t t)
    )
  )
(defun jg-tag-unify-layer/open-selection (pair)
  "Open only a selection of a large file "
  (let ((file (car pair))
        (selection-size (cdr pair))
        selection)
    (with-temp-buffer
      (insert-file file)
      (goto-char (random (- (point-max) selection-size)))
      (setq selection (buffer-substring (point) (+ (point) selection-size)))
      )
    (with-temp-buffer-window (format "*%s - selection*" (-last-item (f-split file)))
                             nil nil
                             (princ selection)
                             )
    )
  )
(defun jg-tag-unify-layer/display-selection (n)
  "Open only a selection of large files "
  (interactive "nNum Chars: ")
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/open-selection
              (-zip-fill n files '()))
    )
  )

