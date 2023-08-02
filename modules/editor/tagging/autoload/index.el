;; Indexing

;;;###autoload
(defun +jg-tag-index-people ()
  "Index all twitter users in the current directory "
  (interactive)
  ;; Get all org files
  (let ((all-orgs (directory-files-recursively (dired-current-directory) "\.org$"))
        (index-hash (make-hash-table :test 'equal))
        (inserted-for-file (make-hash-table :test 'equal))
        (curr-d (dired-current-directory))
        )
    (message "Found %s org files" (length all-orgs))
    ;; For each org collect people in file
    (cl-loop for filename in all-orgs
             do (with-temp-buffer
                  (clrhash inserted-for-file)
                  (insert-file filename)
                  (goto-char (point-min))
                  (while (re-search-forward "\*+ \\(@[_[:word:]]+\\)" nil t)
                    (let ((str (match-string 1)))
                      (if (null (gethash str inserted-for-file))
                          (progn (puthash str t inserted-for-file)
                                 (if (null (gethash str index-hash))
                                     (puthash str '() index-hash))
                                 ;; (message "Pushing %s : %s" str filename)
                                 (push filename (gethash str index-hash))))))))
    ;; create index
    (message "Accumulated %s accounts" (length (hash-table-keys index-hash)))
    (with-temp-buffer
      (maphash (lambda (k v)
                 (insert (format "%s "k))
                 (mapc (lambda (x)
                         (insert (format ":%s" x))) v)
                 (insert "\n")
                 ) index-hash)
      (write-file jg-tag-loc-twitter-account-index t)
      )
    )
  (message "Finished writing file")
  )

;;;###autoload
(defun +jg-tag-index-tags()
  " Run routine to index all tags in org files "
  (interactive)
  ;; Get all org files
  (let ((all-orgs (directory-files-recursively (dired-current-directory) "\.org$"))
        (index-hash (make-hash-table :test 'equal))
        (inserted-for-file (make-hash-table :test 'equal))
        (curr-d (dired-current-directory))
        )
    (message "Found %s org files" (length all-orgs))
    ;; For each org collect tags in file
    (cl-loop for filename in all-orgs
             do (with-temp-buffer
                  (org-mode)
                  (clrhash inserted-for-file)
                  (insert-file filename)
                  (goto-char (point-min))
                  ;;Get tags:
                  (while (re-search-forward "^\\*\\* " nil t)
                    (let ((tags (org-get-tags nil t)))
                      (mapc (lambda (x)
                              (if (null (gethash x inserted-for-file))
                                  (progn (puthash x t inserted-for-file)
                                         (if (null (gethash x index-hash))
                                             (puthash x '() index-hash))
                                         (push filename (gethash x index-hash))
                                         )
                                )
                              )
                            tags)
                      )
                    )
                  )
             )
    ;; create index
    (message "Accumulated %s tags" (length (hash-table-keys index-hash)))
    (with-temp-buffer
      (maphash (lambda (k v)
                 (insert (format "%s "k))
                 (mapc (lambda (x)
                         (insert (format ":%s" x))) v)
                 (insert "\n")
                 ) index-hash)
      (write-file jg-tag-loc-twitter-tag-index t)
      )
    )
  (message "Finished writing file")
  )
