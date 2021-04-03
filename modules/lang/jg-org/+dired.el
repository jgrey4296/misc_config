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
(defun +jg-org-dired-clean-remove-surplus ()
  " Remove additional single star headings in an org file "
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          do
          (with-temp-buffer
            (insert-file-contents file)
            (indent-region (point-min) (point-max))
            (+jg-org-remove-surplus-headings)
            (+jg-org-sort-headings)
            (+jg-org-remove-duplicate-tweet-entries)
            (+jg-org-clean-whole-duplicate-threads)
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
  )
(defun +jg-org-sort-headings ()
  " Call org-sort-entries on a buffer "
  (goto-char (point-min))
  (org-mode)
  (org-show-all)
  (org-sort-entries nil ?a)
  )
(defun +jg-org-clean-whole-duplicate-threads ()
  " Call org-sort-entries on a buffer "
  (goto-char (point-min))
  (org-mode)
  (org-show-all)
  ;; map over level 2 subtrees
  ;; if every applicable heading is a duplicate link,
  ;; mark it for removal the entire subtree
  (defvar jg-dup-2-star (make-marker))
  (defvar jg-dup-3-star (make-marker))
  (defvar jg-dup-hash-log (make-hash-table))

  (org-map-entries '+jg-org-map-entry-duplicate-finder t nil)
  ;; remove marked subtrees
  (let* ((filtered-keys (-filter #'(lambda (x) (gethash x jg-dup-hash-log))
                                 (hash-table-keys jg-dup-hash-log)))
         (sorted-keys (sort filtered-keys #'>))
         )
    (loop for pos in sorted-keys
          do
          (goto-char pos)
          (org-cut-subtree))
    )

  (makunbound 'jg-dup-2-star)
  (makunbound 'jg-dup-3-star)
  (makunbound 'jg-dup-hash-log)

  (sleep-for 0.2)
  )
(defun +jg-org-map-entry-duplicate-finder ()
  (let ((ctx (org-element-context))
        (props (org-entry-properties)))
    (cond ((alist-get "PERMALINK" props nil nil #'s-equals?)
           (puthash (marker-position jg-dup-2-star) nil jg-dup-hash-log)
           (puthash (marker-position jg-dup-3-star) nil jg-dup-hash-log)
           )
          ((s-contains? "Conversations" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((s-contains? "Links" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((s-contains? "Media" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((s-contains? "Videos" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((eq (plist-get (cadr ctx) :level) 2)
           (move-marker jg-dup-2-star (plist-get (cadr ctx) :begin))
           (puthash (marker-position jg-dup-2-star) t jg-dup-hash-log)
           (move-marker jg-dup-3-star jg-dup-2-star)
           )
          ((eq (plist-get (cadr ctx) :level) 3)
           (move-marker jg-dup-3-star (plist-get (cadr ctx) :begin))
           (puthash (marker-position jg-dup-3-star) t jg-dup-hash-log))
          )
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
