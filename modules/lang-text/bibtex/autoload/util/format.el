;;; +format.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-cleanup-ensure-newline-before-def ()
  (while (re-search-forward "\\(\n\\)\\(@.+?{.+?,\\)$" nil t)
    (goto-char (match-end 1))
    (insert "\n")
    (goto-char (match-end 0))
    )
  )
