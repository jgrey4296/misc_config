;;; flycheck.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +jg-python-lsp-flycheck-filter (params workspace)
  " Filters out diagnostic messages about unused imports
  params is a hash-table, params[diagnostics] is a vector of hash-tables,
each hash-table is a pyright report.
unused imports are the only ones with tags

Available Keys: (range message severity code source data)

"
  ;; (message "Starting filter")
  (clrhash (lsp--workspace-diagnostics workspace))
  (let ((diags (gethash "diagnostics" params))
        (new-diags nil)
        )
    (remhash "diagnostics" params)
    (cl-loop for table in (append diags nil)
             do
             ;; (message "Diag: %s : %s : %s : %s"
             ;;          (gethash "source" table)
             ;;          (gethash "severity" table)
             ;;          (s-replace-regexp "\n+" " " (gethash "message" table))
             ;;          (gethash "tags" table)
             ;;          )
             (cond ((and (equal "pyright" (downcase (gethash "source" table)))
                         (gethash "tags" table))
                    ;; (message "Skipping")
                    nil
                    )
                   ;; ((and (equal "ruff" (downcase (gethash "source" table)))
                   ;;       (not (eq (gethash "tags" table) [1])))
                   ;;  nil
                   ;;  )
                   ((equal "ruff" (downcase (gethash "source" table)))
                    (remhash "tags" table)
                    (puthash "message" (format "Ruff: %s" (gethash "message" table)) table)
                    (push table new-diags)
                    )
                   (t
                    ;; (message "Default")
                    (puthash "message" (format "%s: %s"
                                               (gethash "source" table)
                                               (s-replace-regexp "\n+" " "
                                                                 (gethash "message" table))
                                               )
                             table)
                    (push table new-diags)
                    )
                   )
             )
    (puthash "diagnostics" new-diags params)
    params
    )
  )
