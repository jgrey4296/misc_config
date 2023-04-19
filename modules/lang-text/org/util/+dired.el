;;; lang/jg-org/+dired.el -*- lexical-binding: t; -*-

(defun +jg-org-display-selection (n)
  "Open only a selection of large files "
  (interactive "nNum Chars: ")
  (let ((files (dired-get-marked-files)))
    (seq-each '+jg-tag-open-selection
              (-zip-fill n files '()))
    )
  )
(defun +jg-org-dired-clean ()
  " Remove Surplus headings, sort, remove duplicate tweets,
remove empty threads "
  (interactive)
  (let ((files (-filter #'(lambda (x) (f-ext? x "org")) (dired-get-marked-files)))
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
