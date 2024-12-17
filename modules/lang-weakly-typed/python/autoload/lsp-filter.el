;;; lsp-filter .el -*- lexical-binding: t; -*-

(defvar jg-lsp-message-filter nil)

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
             when jg-lsp-message-filter do
             (message "Diag: %s : %s : %s : %s"
                      (gethash "source" table)
                      (gethash "severity" table)
                      (s-replace-regexp "\n+" " " (gethash "message" table))
                      (gethash "tags" table)
                      )
             do
             (cond ((and (equal "pyright" (downcase (gethash "source" table)))
                         (gethash "tags" table))
                    (when jg-lsp-message-filter (message "Skipping"))
                    nil
                    )
                   ((equal "ruff" (downcase (gethash "source" table)))
                    (remhash "tags" table)
                    (puthash "message" (format "Ruff: %s" (gethash "message" table)) table)
                    (push table new-diags)
                    )
                   (t
                    (when jg-lsp-message-filter (message "Default"))
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
