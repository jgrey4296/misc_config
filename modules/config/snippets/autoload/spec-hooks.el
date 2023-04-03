;; -*- mode:emacs-lisp; no-byte-compile: t;  -*-

;; Definee and reapply file specs

;;;###autodef
(defun +jg-snippets-reapply-file-specs ()
  " Activate stored file templates, and ensure the correct snippet directories are set  "
  (interactive)
  (let* ((all-rules (copy-sequence (-flatten-n 1 (hash-table-values jg-snippets-file-specs))))
         (flattened (-concat (mapcar #'cdr (sort all-rules #'(lambda (x y) (< (car x) (car y)))))
                             '(("*jg-modified*"))))
         )
    (message "Activating File Templates: %s" (hash-table-keys jg-snippets-file-specs))
    (setq +file-templates-dir jg-snippets-file-templates-dir
          +snippets-dir       jg-snippets-code-templates-dir
          yas-snippet-dirs (list +snippets-dir
                                 +file-templates-dir
                                 doom-snippets-dir
                                 yasnippet-snippets-dir)
          yas--default-user-snippets-dir jg-snippets-code-templates-dir
          +file-templates-alist flattened
          )
    (provide 'jg-snippets-applied)
    )
  )

;;;###autodef
(defun +jg-snippets-add-file-spec (sym rules &optional override)
  " define a set of file template rules, under the symbol `sym` to avoid duplicates  "
  (cl-assert (hash-table-p jg-snippets-file-specs))
  (unless (and (gethash sym jg-snippets-file-specs) (not override))
    (puthash sym
             (cl-loop for (head . body) in rules
                      for priority = (* -1 (or (plist-get body :priority) 0))
                      for clean    = (cl-loop for (k v) on body by #'cddr
                                              unless (eq k :priority)
                                              collect k and collect v)
                      collect (cons priority (cons head clean))
                      )
             jg-snippets-file-specs)
    )
  )
