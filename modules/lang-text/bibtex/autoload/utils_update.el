;;; utils_update.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-update-entry ()
  (interactive)
  (when (org-ref-bibtex-entry-doi)
    (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi))
    )
  )
