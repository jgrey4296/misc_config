;;; org/jg-org/+org-ref-funcs.el -*- lexical-binding: t; -*-


(defun +jg-org-bibtex-load-random ()
  """ Run in a bibtex file, opens a random entry externally,
      and logs it has been opened in a separate file """
  (interactive)
  (widen)
  (let* ((location (f-dirname (buffer-file-name)))
         (log_file (f-join location ".emacs_rand_bib_log"))
         (log_hash (if (f-exists? log_file) (with-temp-buffer
                                              (insert-file-contents log_file)
                                              (let ((uf (make-hash-table :test 'equal)))
                                                (seq-each (lambda (x) (puthash x 't uf)) (split-string (buffer-string) "\n"))
                                                uf))
                     (make-hash-table :test 'equal)))
         )
    ;; go to random line
    (goto-char (random (point-max)))
    (org-ref-bibtex-next-entry)
    (let ((entry (bibtex-parse-entry)))
      (while entry
        (if (gethash (alist-get "=key=" entry nil nil 'equal) log_hash)
            (progn (goto-char (random (point-max)))
                   (org-reg-bibtex-next-entry)
                   (setq entry (bibtex-parse-entry)))
          (progn
            (write-region (alist-get "=key=" entry nil nil 'equal)
                          nil log_file 'append)
            (write-region "\n" nil log_file 'append)
            (bibtex-narrow-to-entry)
            (goto-char (point-min))
            (org-open-link-from-string (message "[[%s]]" (bibtex-text-in-field "file")))
            (setq entry nil)
            )
          )
        )
      )
    )
  )
(defun +jg-org-ref-open-bibtex-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (let* ((file (bibtex-autokey-get-field "file"))
           (optfile (bibtex-autokey-get-field "OPTfile")))
      (message "%s : %s" file (file-exists-p file))
      (if (and (not (string-equal "" file)) (file-exists-p file))
          (org-link-open-from-string (format "[[file:%s]]" file))
        (progn (message optfile)
               (org-link-open-from-string (format "[[file:%s]]" optfile)))))))



(defun +jg-org-ref-search-scholar (pre)
  (interactive "p")
  (message "P: %s" pre)
  (save-excursion
    (let* ((bibtex-parse-strings t)
           (fields '("author" "title" "year"))
           (retrieved (-reject '(lambda (x) (string-equal "" x)) (mapcar 'bibtex-autokey-get-field fields)))
           (search-string (string-join retrieved "+"))
           (fallback (if (or (> pre 1)
                             (string-equal "" search-string))
                         (replace-regexp-in-string " " "+" (read-string "Search for: "))
                       nil))
           )
      (message "Scholar Search: %s : %s" search-string fallback)
      (browse-url (format jg-scholar-search-string (if fallback fallback search-string)))
      )
    )
  )
