;;; lang/jg-org/+dired.el -*- lexical-binding: t; -*-

(defun +jg-org-dired-clean-marked-files ()
  "Clean marked Org files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each '+jg-org-clean-with-backup files)
    )
  )
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

(defun +jg-org-dired-remove-duplicates ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          do
          (message "Removing Duplicates in %s" file)
          (with-temp-buffer
            (insert-file-contents file t)
            (org-mode)
            (+jg-org-remove-duplicates)
            (write-file file)
            )
    )
  )
)
