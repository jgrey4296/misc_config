;;; lang/jg-org/+dired.el -*- lexical-binding: t; -*-


(defun +jg-org-quick-compress-orgs ()
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
(defun +jg-org-display-selection (n)
  "Open only a selection of large files "
  (interactive "nNum Chars: ")
  (let ((files (dired-get-marked-files)))
    (seq-each '+jg-tag-open-selection
              (-zip-fill n files '()))
    )
  )
(defun +jg-org-chop-long-files-from-dired ()
  "Subdivide marked files if they are too long"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each '+jg-org-chop-long-file files)
    )
  )
(defun +jg-org-dired-fix-org-links ()
  " fix links to local files "
  (interactive)
  (let* ((files (dired-get-marked-files))
         (orgs (-filter #'(lambda (x) (string-equal "org" (f-ext x))) files))
         )
    (cl-loop for file in orgs do
          (with-temp-buffer
            (insert-file file)
            (while (re-search-forward "\\[\\[.+\\(/.+?_files\\)" nil t)
              (replace-match "[[file:.\\1")
              )
            (write-file file)
            )
          )
    )
  )

(defun +jg-org-dired-clean ()
  " Remove Surplus headings, sort, remove duplicate tweets,
remove empty threads "
  (interactive)
  (let ((files (dired-get-marked-files))
        failures)
    (cl-loop for file in files
          do
          (with-temp-buffer
            (condition-case err
                (progn
                  (message "---------- Removing Surplus on: %s" file)
                  (insert-file-contents file)
                  (org-mode)
                  (+jg-org-clean-master)
                  (write-file file))
              ;; Record Errors
              (error (push file failures)))
            )
          )
    (message "Removed Surplus except for: %s" failures)
    )
  )

(defun +jg-org-dired-select-org ()
  (interactive)
  (dired-mark-if
   (let ((fn (dired-get-filename 'no-dir t)))
     (and fn (s-matches? ".+?\.org$" fn))
    )
   "matching org")
  )
(defun +jg-org-dired-add-twitter-prop()
  "Clean marked Org files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (cl-loop for file in files
          do
          (with-temp-buffer
            (insert-file-contents file t)
            (org-mode)
            (+jg-org-add-twitter-property)
            (write-file file)
            )
    )
  )
)
