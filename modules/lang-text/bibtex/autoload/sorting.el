;; sorting.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-sort-buffer-by-year ()
  (interactive)
  (let ((bibtex-autokey-year-length 4)
        (bibtex-maintain-sorted-entries (list
                                         #'(lambda () (string-to-number (+jg-bibtex-autokey-get-year)))
                                         #'<)))
    (bibtex-sort-buffer)
    )
  )

;;;###autoload
(defun +jg-bibtex-autokey-get-year ()
  "Return year field contents as a string obeying `bibtex-autokey-year-length'."
  (let* ((str (bibtex-autokey-get-field '("date" "year"))) ; possibly ""
         (year (or (and (iso8601-valid-p str)
                        (let ((year (decoded-time-year (iso8601-parse str))))
                          (and year (number-to-string year))))
                   ;; BibTeX permits a year field "(about 1984)", where only
                   ;; the last four nonpunctuation characters must be numerals.
                   (and (string-match "\\([0-9][0-9][0-9][0-9]\\)[^[:alnum:]]*\\'" str)
                        (match-string 1 str))
                   (user-error "%s : Year or date field `%s' invalid" (bibtex-autokey-get-title) str))))
    (substring year (max 0 (- (length year) bibtex-autokey-year-length)))))
