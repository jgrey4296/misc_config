;;; +subcite.el -*- lexical-binding: t; -*-


(defun +jg-bibtex-lock-key ()
  (interactive)
  (bibtex-beginning-of-entry)
  (let ((key (bibtex-completion-get-key-bibtex)))
    (unless (s-matches? "_$" key)
      (re-search-forward "{" (line-end-position))
      (kill-line)
      (insert (concat key "_,"))
      )
    )
  )

(defun +jg-bibtex-subcite ()
  (interactive)
  (+jg-bibtex-lock-key)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (key (alist-get "=key=" entry nil nil #'s-equals?))
         (type (alist-get "=type=" entry nil nil #'s-equals?))
         (year (alist-get "year" entry nil nil #'s-equals?))
         (cite-type (concat "@In" type))
         )
    ;; make key permanent if necessary

    ;; go to end of entry
    ;; insert stub
    (bibtex-end-of-entry)
    (insert "\n\n")
    (insert cite-type "{stub-entry,\n")
    (insert "year = " year ",\n")
    (insert "crossref = {" key "},\n")
    (insert "title = {},\n")
    (insert "author = {},\n")
    (insert "}")
    (bibtex-beginning-of-entry)
    )
  )