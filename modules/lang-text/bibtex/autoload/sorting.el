;; sorting.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-sort-buffer-by-year ()
  (interactive)
  (let ((bibtex-autokey-year-length 4)
        (bibtex-maintain-sorted-entries (list
                                         #'(lambda () (string-to-number (bibtex-autokey-get-year)))
                                         #'<)))
    (bibtex-sort-buffer)
    )
  )
