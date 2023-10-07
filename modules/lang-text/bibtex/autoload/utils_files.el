;;; +files.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +jg-bibtex-open-pdf (&optional path)
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (let* ((target (if path path (bibtex-autokey-get-field '("file" "OPTfile"))))
           )
      (message "Bibtex Open Target: %s" target)
      (cond ((string-empty-p target)
             (message "No File to open"))
            ((not (file-exists-p target))
             (message "File does not exist"))
            (t (call-process "open" nil nil nil target))
            )

      (when jg-bibtex-open-doi-with-pdf
          (+jg-bibtex-open-doi))
      (when jg-bibtex-open-url-with-pdf
          (+jg-bibtex-open-url))
      )))

;;;###autoload
(defun +jg-bibtex-open-url ()
  " Open the current entry's url in browser "
  (interactive)
  (when (bibtex-text-in-field "url")
    (browse-url (bibtex-text-in-field "url")))
  )

;;;###autoload
(defun +jg-bibtex-open-doi ()
  " Follow the doi link of the current entry in a browser "
  (interactive)
  (when (bibtex-text-in-field "doi")
    (browse-url (format jg-bibtex-doi-url (bibtex-text-in-field "doi")))
    )
  )

;;;###autoload
(defun +jg-bibtex-open-folder ()
  " Open the associated file's folder in finder "
  (interactive)
  (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
    (when (and (not (string-empty-p target)) (f-exists? target))
      (message "Opening %s" target)
      (shell-command (concat "open " (shell-quote-argument (f-parent target))))
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-load-random ()
  " Run in a bibtex file, opens a random entry externally,
      and logs it has been opened in a separate file.

Log into jg-bibtex-rand-log.
 "
  (interactive)
  (widen)
  (let* ((location (f-dirname (buffer-file-name)))
         (log_file (f-join location jg-bibtex-rand-log))
         (log_hash (if (f-exists? log_file)
                       (with-temp-buffer
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
            (+jg-bibtex-open-pdf)
            (setq entry nil)
            )
          )
        )
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-quicklook-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
      (message "%s : %s" target (file-exists-p target))
      (browse-url target 'quicklook)
      )))
