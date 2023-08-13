;;; +clean_funcs.el -*- lexical-binding: t; -*-
(require 'bibtex)

;;;###autoload
(defun +jg-bibtex-clean-doi-hook ()
  "Remove http://dx.doi.org/ in the doi field.
Used instead of org-ref-bibtex-format-url-if-doi
and orcb-clean-doi
"
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (ffap-url-p  doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http.*?\.org/" "" doi)))))

;;;###autoload
(defun +jg-bibtex-isbn-clean ()
  (let ((isbn (bibtex-autokey-get-field "isbn")))
    (unless (string-empty-p isbn)
      (bibtex-set-field "isbn" (s-replace-regexp "[[:blank:]]+" "-" (s-trim isbn)))
      ))
  )
