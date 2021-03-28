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
    (loop for file in orgs do
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

(defun +jg-org-dired-remove-duplicate-tweets ()
  " Find duplicate tweets by permalink, replace with org link
to the first version found "
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          do
          (message "Removing Duplicates in %s" file)
          (with-temp-buffer
            (insert-file-contents file t)
            (org-mode)
            (+jg-org-remove-duplicate-tweet-entries)
            (write-file file)
            )
    )
  )
)
(defun +jg-org-dired-clean-marked-files ()
  "Clean marked Org files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          do
          (message "Cleaning in %s" file)
          (with-temp-buffer
            (insert-file-contents file t)
            (org-mode)
            (+jg-org-clean)
            (write-file file)
            )
    )
  )
)


(defun +jg-org-dired-clean-remove-surplus-headings ()
  " Remove additional single star headings in an org file "
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          do
          (with-temp-buffer
            (insert-file-contents file)
            (+jg-org-remove-surplus-headings)
            (write-file file)
            )
          )
    )
  )
(defun +jg-org-remove-surplus-headings ()
  " Go through a buffer, removing additional single star headings,
and the property block directly below "
  (goto-char (point-min))
  (forward-line)
  (let ((kill-whole-line t))
    (while (re-search-forward "^\* " nil t)
      ;; Delete the heading
      (beginning-of-line)
      (kill-line)
      ;; if theres a property block, delete it
      (while (looking-at-p "^[[:space:]]*:.+?:")
        (kill-line)
        )
      )
    )
  (sleep-for 0.2)
  )

(defun +jg-org-dired-clean-sort-headings ()
  " Call org-sort-entries on each dired marked file's content "
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          do
          (with-temp-buffer
            (insert-file-contents file)
            (+jg-org-sort-headings)
            (write-file file)
            )
          )
    )
  )
(defun +jg-org-sort-headings ()
  " Call org-sort-entries on a buffer "
  (goto-char (point-min))
  (org-mode)
  (org-show-all)
  (org-sort-entries nil ?a)
  (sleep-for 0.2)
  )

(defun +jg-org-dired-clean-whole-duplicate-threads ()
  " Call org-sort-entries on each dired marked file's content "
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          do
          (with-temp-buffer
            (insert-file-contents file)
            (+jg-org-clean-whole-duplicate-threads)
            (write-file file)
            )
          )
    )
  )
(defun +jg-org-clean-whole-duplicate-threads ()
  " Call org-sort-entries on a buffer "
  (goto-char (point-min))
  (org-mode)
  (org-show-all)
  ;; map over level 2 subtrees
  ;; if every applicable heading is a duplicate link,
  ;; mark it for removal the entire subtree

  ;; remove marked subtrees

  (sleep-for 0.2)
  )
