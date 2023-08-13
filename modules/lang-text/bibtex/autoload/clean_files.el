;;; +files.el -*- lexical-binding: t; -*-
(require 'bibtex)

;;;###autoload
(defun +jg-bibtex--get-file-entries (pair)
  (if (string-match "file" (car pair))
      pair
    nil)
  )

;;;###autoload
(defun +jg-bibtex--check-file-exists (pair)
  (let* ((orig (cdr pair))
         (sub (substring
               (cdr pair)
               1 -1))
         (full-target (if (f-relative? sub)
                          (f-join jg-bibtex-pdf-loc sub)
                        (expand-file-name sub))))
    (cl-assert (eq (string-to-char orig) ?{))
    (if (not (f-exists? full-target))
        (signal 'error `("File Not Found: " ,full-target))))
  )

;;;###autoload
(defun +jg-bibtex-check-file-hook ()
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (file-likes (-filter 'identity (mapcar #'+jg-bibtex--get-file-entries entry)))
        )
    (mapc #'+jg-bibtex--check-file-exists file-likes)
    )
  )
