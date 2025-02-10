;;; completion/company/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-company-try-backend ()
  (interactive)
  (let ((selected (ivy-read "Select Backend: " jg-company-known-backends :require-match t))
         )
    (company-begin-backend (intern selected))
    )
  )
