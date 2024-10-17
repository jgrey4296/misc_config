;;; +format.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-cleanup-ensure-newline-before-def ()
  (while (re-search-forward "\\(\n\\)\\(@.+?{.+?,\\)$" nil t)
    (goto-char (match-end 1))
    (insert "\n")
    (goto-char (match-end 0))
    )
  )

;;;###autoload
(defun +jg-bibtex-cleanup-sort-entry ()
  "Use org-ref-sort-bibtex-entry, but narrowed to the entry"
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (condition-case err
        (org-ref-sort-bibtex-entry)
      (error (message "Error: %s" err))
      )
    (bibtex-beginning-of-entry)
    (org-ref-clean-bibtex-entry)
    )
  )

;;;###autoload
(defun +jg-bibtex-kill-entry-key ()
  (interactive)
  (bibtex-beginning-of-entry)
  (let ((key (bibtex-completion-get-key-bibtex)))
    (unless (s-matches? "_$" key)
      (re-search-forward "{" (line-end-position))
      (kill-line)
      (insert ",")
      )
    )
  )
