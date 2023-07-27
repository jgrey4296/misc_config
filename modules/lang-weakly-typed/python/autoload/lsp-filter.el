;;; flycheck.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-lsp-flycheck-filter (params workspace)
  " Filters out diagnostic messages about unused imports
  params is a hash-table, params[diagnostics] is a vector of hash-tables,
each hash-table is a pyright report.
unused imports are the only ones with tags
"
  (let ((diags (gethash "diagnostics" params))
        (new-diags nil)
        )
    (cl-loop for table in (append diags nil)
             if (not (gethash "tags" table))
             do
             (push table new-diags)
             )
    (puthash "diagnostics" (vconcat new-diags) params)
    params
    )
  )
