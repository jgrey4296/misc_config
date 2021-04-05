;; bibtex

(defun +jg-bibtex-build-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (setq bibtex-completion-bibliography (directory-files jg-bibtex-loc-bibtex 't "\.bib$")))

(defun +jg-bibtex-unify-pdf-locations-in-file (name)
  "Change all pdf locations in bibtex file to relative,
ensuring they work across machines "
  (message "Unifying Locations in %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (while (re-search-forward jg-bibtex-pdf-loc-regexp nil t)
      (replace-match jg-bibtex-pdf-replace-match-string nil nil nil 1)
      (if (eq 6 (length (match-data)))
          (replace-match jg-bibtex-pdf-replace-library-string t nil nil 2))
      )
    (write-file name)
    )
  )
(defun +jg-bibtex-dired-unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each '+jg-bibtex-unify-pdf-locations-in-file files)
    )
  )

(defun +jg-bibtex-google-scholar ()
  "Open the bibtex entry at point in google-scholar by its doi."
  (interactive)
  (let* ((search-texts (mapcar #'bibtex-autokey-get-field jg-bibtex-scholar-search-fields))
         (exact-texts (mapcar #'bibtex-autokey-get-field jg-bibtex-scholar-search-fields-exact))
         (exact-string (s-join " " (mapcar #'(lambda (x) (format "\"%s\"" x))
                                           (-filter #'(lambda (x) (not (string-empty-p x))) exact-texts))))
         (all-terms (s-concat exact-string " " (s-join " " search-texts)))
         (search-string (format jg-bibtex-scholar-search-string all-terms))
         )
    (browse-url search-string)
    )
  )
(defun +jg-bibtex-edit-entry-type ()
  " Edit the @type of a bibtex entry, using
bibtex-BibTeX-entry-alist for completion options "
  (interactive)
  (let* ((type-options (mapcar 'car bibtex-BibTeX-entry-alist))
         (selection (completing-read "New Bibtex Type: " type-options))
         )
    (save-excursion
      (bibtex-beginning-of-entry)
      (when (search-forward-regexp "@\\(.+?\\){" (line-end-position))
        (replace-match (string-trim selection) t nil nil 1)
        )
      )
    )
  )
(defun +jg-bibtex-copy-entry ()
  " Copy the entire entry under point "
  (interactive)
  (save-excursion
    (let ((start (bibtex-beginning-of-entry))
          (end (bibtex-end-of-entry)))
      (copy-region-as-kill start end)
      )
    )
  )
(defun +jg-bibtex-copy-key ()
  " Copy the cite key of the entry under point "
  (interactive)
  (kill-new (bibtex-completion-get-key-bibtex))
  )
(defun +jg-bibtex-open-pdf ()
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
(defun +jg-bibtex-find-folder ()
  " Find the fold in which the entry's associated file exists "
  (interactive)
  (let* ((file (bibtex-autokey-get-field "file"))
         (optfile (bibtex-autokey-get-field "OPTfile"))
         (target (if (not (string-empty-p file)) file optfile))
        )
    (if (and (not (string-empty-p target)) (f-exists? (f-parent target)))
        (progn
          (message "Opening %s" target)
          (find-file-other-window (f-parent target))
          )
      )
    )
  )
(defun +jg-bibtex-open-folder ()
  " Open the associated file's folder in finder "
  (interactive)
  (let* ((file (bibtex-autokey-get-field "file"))
         (optfile (bibtex-autokey-get-field "OPTfile"))
         (target (if (not (string-empty-p file)) file optfile))
        )
    (if (and (not (string-empty-p target)) (f-exists? target))
        (progn
          (message "Opening %s" target)
          (shell-command (format "open %s" (f-parent target)))
          )
      )
    )
  )
(defun +jg-bibtex-open-url ()
  " Open the current entry's url in browser "
  (interactive)
  (when (bibtex-text-in-field "url")
    (browse-url (bibtex-text-in-field "url")))
  )
(defun +jg-bibtex-open-doi ()
  " Follow the doi link of the current entry in a browser "
  (interactive)
  (when (bibtex-text-in-field "doi")
    (browse-url (format "https://doi.org/%s" (bibtex-text-in-field "doi")))
    )
  )
(defun +jg-bibtex-refile-by-year ()
  " Kill the current entry and insert it in the appropriate year's bibtex file "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((year (bibtex-text-in-field "year"))
         (year-file (format "%s.bib" year))
         (bib-path jg-bibtex-loc-bibtex)
         (response (if year (read-string (format "Refile to %s? " year-file))))
         (target (if (and year (or (s-equals? "y" response) (string-empty-p response)))
                     (f-join bib-path year-file)
                   (completing-read "Bibtex file: "
                                    (f-entries bib-path
                                               (lambda (f) (f-ext? f "bib"))))))
         )
      (bibtex-kill-entry)
      (with-temp-buffer
        (if (f-exists? target)
            (insert-file-contents target))
        (goto-char (point-max))
        (insert "\n")
        (bibtex-yank)
        (write-file target nil)
      )
    )
  )
(defun +jg-bibtex-visual-select-entry ()
  " Evil visual select the current entry "
  (interactive)
  (evil-visual-make-region (bibtex-beginning-of-entry)
                           (bibtex-end-of-entry))
)
(defun +jg-bibtex-goto-crossref-entry ()
  " Follow the crossref field in the entry "
  (interactive)
  (when (bibtex-text-in-field "crossref")
    (bibtex-find-crossref (bibtex-text-in-field "crossref"))
    )
  )
(defun +jg-bibtex-edit-field ()
  " Edit a specified field in the current entry,
using org-bibtex-fields for completion options "
  ;; TODO add completion's for fields in completion loc
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((composition (-compose #'(lambda (x) (s-replace ":" "" x)) #'symbol-name #'car  ))
           (fields (mapcar composition org-bibtex-fields))
           (user-fields (mapcar #'car bibtex-user-optional-fields))
           (chosen (completing-read "Field: " (-concat fields user-fields)))
           (curr-value (bibtex-autokey-get-field chosen))
           (new-value (read-string (format "(%s) New Value: " chosen)))
           )
      (bibtex-set-field chosen (string-trim new-value))
      )
    )
  )

(defun +jg-bibtex-load-random ()
  " Run in a bibtex file, opens a random entry externally,
      and logs it has been opened in a separate file.

Log into jg-bibtex-rand-log.
 "
  (interactive)
  (widen)
  (let* ((location (f-dirname (buffer-file-name)))
         (log_file (f-join location jg-bibtex-rand-log))
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
            (+jg-bibtex-open-pdf)
            (setq entry nil)
            )
          )
        )
      )
    )
  )
(defun +jg-bibtex-clean-entry ()
  """ Calls org-ref-clean-bibtex-entry,
  but with a wrapping to override fill-column
  """
  (interactive)
  (condition-case err
      (let ((fill-column jg-bibtex-fill-column))
        (org-ref-clean-bibtex-entry)
        )
    (error
     (if jg-bibtex-clean-move-entry-on-fail
         (let (entry)
           (kill-region (bibtex-beginning-of-entry)
                        (bibtex-end-of-entry))
           (setq entry (string-trim (current-kill 0 t)))
           (widen)
           (save-excursion
             (goto-char (point-max))
             (insert entry)
             (insert "\n")
             )
           (message "Clean Error, copied entry to end of file")
           )
       (message "Clean Error: %s" (error-message-string err))
       )
     )
    )
  )

(defun +jg-bibtex-clean-error-move-toggle ()
  (interactive)
  (setq jg-bibtex-clean-move-entry-on-fail (not jg-bibtex-clean-move-entry-on-fail))
  (message "Error on clean entry %s move to end of file" (if jg-bibtex-clean-move-entry-on-fail
                                                             "will"
                                                           "will not"))
  )
