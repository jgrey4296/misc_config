;;; +ui.el -*- lexical-binding: t; -*-
(require 'bibtex)

;;;###autoload
(defun +jg-bibtex-toggle-doi-load ()
  (interactive)
  (setq jg-bibtex-open-doi-with-pdf (not jg-bibtex-open-doi-with-pdf))
  (message "Open DOI on pdf? %s" jg-bibtex-open-doi-with-pdf)
  )

;;;###autoload
(defun +jg-bibtex-toggle-url-load ()
  (interactive)
  (setq jg-bibtex-open-url-with-pdf (not jg-bibtex-open-url-with-pdf))
  (message "Open URL on pdf? %s" jg-bibtex-open-url-with-pdf)
  )

;;;###autoload
(defun +jg-bibtex-visual-select-entry ()
  " Evil visual select the current entry "
  (interactive)
  (evil-visual-make-region (bibtex-beginning-of-entry)
                           (bibtex-end-of-entry))
)

;;;###autoload
(defun +jg-bibtex-goto-crossref-entry ()
  " Follow the crossref field in the entry "
  (interactive)
  (when (bibtex-text-in-field "crossref")
    (bibtex-find-crossref (bibtex-text-in-field "crossref"))
    )
  )

;;;###autoload
(defun +jg-bibtex-tweet-random-entry ()
  (interactive)
  (unless bibtex-completion-bibliography
      (+jg-bibtex-build-list))
  ;; TODO : limit to a range of years
  (let ((chosen-file (nth (random (length bibtex-completion-bibliography))
                          bibtex-completion-bibliography))
        (log-file (f-join jg-bibtex-loc-bibtex jg-bibtex-tweet-rand-log))
        entry
        )
    (with-temp-buffer
      ;; TODO could search for doi's, then move a random number of those matches
      (insert-file-contents chosen-file)
      (goto-char (point-min))
      (forward-char (random (point-max)))
      (bibtex-previous-entry)
      (setq entry (bibtex-parse-entry)))
      (+jg-twitter-tweet-with-input (format jg-bibtex-tweet-pattern
                                            (alist-get "year" entry nil nil #'equal)
                                            (alist-get "title" entry nil nil #'equal)
                                            (alist-get "author" entry nil nil #'equal)
                                            (alist-get "tags" entry nil nil #'equal)
                                            (or (alist-get "doi" entry nil nil #'equal)
                                                (alist-get "url" entry nil nil #'equal)
                                                (alist-get "isbn" entry nil nil #'equal))
                                            ))
      (write-region (format "%s\n" (alist-get "=key=" entry nil nil #'equal)) nil
                    log-file t)
    )
  )

;;;###autoload
(defun +jg-bibtex-google-scholar (arg)
  "Open the bibtex entry at point in google-scholar by its doi.
With arg, searchs the dplp instead.
"
  (interactive "P")
  (let* ((search-texts (mapcar #'bibtex-autokey-get-field jg-bibtex-scholar-search-fields))
         (exact-texts  (mapcar #'bibtex-autokey-get-field jg-bibtex-scholar-search-fields-exact))
         (exact-string (s-join " " (mapcar #'(lambda (x) (format "\"%s\"" x))
                                           (-filter #'(lambda (x) (not (string-empty-p x))) exact-texts))))
         (all-terms (s-concat exact-string " " (s-join " " search-texts)))
         (cleaned (s-replace-regexp "{.+?\\(\\w\\)}" "\\1" all-terms))
         )
    (+lookup/online cleaned "Scholar")
    )
  )

;;;###autoload
(defun +jg-bibtex-lookup-orcid (arg)
  (interactive "P")
  (let* ((fields (split-string (bibtex-autokey-get-field '("author" "editor")) " and " t " +"))
         (chosen (ivy-read "Search For: " fields))
         (cleaned (s-replace-regexp "[^[:word:]]+" "+" chosen))
         )
    (+lookup/online cleaned "ORCID")
    )
  )
