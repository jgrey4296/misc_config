;;; lang/jg-org/+dired.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-org-display-selection (n)
  "Open only a selection of large files "
  (interactive "nNum Chars: ")
  (let ((files (dired-get-marked-files)))
    (seq-each '+jg-tag-open-selection
              (-zip-fill n files '()))
    )
  )

;;;###autoload
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

;;;###autoload
(defun +jg-org-dired-select-org ()
  (interactive)
  (dired-mark-if
   (let ((fn (dired-get-filename 'no-dir t)))
     (and fn (s-matches? ".+?\.org$" fn))
    )
   "matching org")
  )

;;;###autoload
(defun +jg-org-dired-export ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (backend (intern (ivy-read "Backend: "
                                   (mapcar #'org-export-backend-name org-export-registered-backends)
                                   :require-match t
                                   )))
        )
    (cl-loop for file in files
             if (f-ext? file "org")
             do
             (with-temp-buffer
               (insert-file file)
               (org-export-to-file backend
                   (f-join (f-parent file) (format "%s.html" (f-base file))))
               )
             )
    )
  )
