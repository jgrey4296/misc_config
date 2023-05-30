;;; domain-specific/bibtex/+motions.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+jg-bibtex-tweet-operator "evil-motions" nil t)
(evil-define-operator +jg-bibtex-tweet-operator (beg end)
  :type line
  (+jg-bibtex-tweet-cmd)
  )

;;;###autoload
(defun +jg-bibtex-tweet-cmd ()
  (interactive)
  (save-excursion
    (let ((author (bibtex-autokey-get-field "author"))
          (title (bibtex-autokey-get-field "title"))
          (year (bibtex-autokey-get-field "year"))
          (tags (bibtex-autokey-get-field "tags"))
          (doi (bibtex-text-in-field "doi"))
          (url (bibtex-text-in-field "url"))
          (isbn (bibtex-text-in-field "isbn")))
      (+jg-twitter-tweet-with-input (format jg-bibtex-tweet-pattern
                                            year title author tags
                                            (or doi url isbn)))
      )
    )
  )
