;; bibtex

(defun +jg-bibtex-build-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (setq bibtex-completion-bibliography (directory-files jg-bibtex-loc-bibtex 't "\.bib$")
        jg-bibtex-helm-candidates nil
        )
  )

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
  (message "Copied Key: %s" (current-kill 0 t))
  )
(defun +jg-bibtex-copy-title ()
  (interactive)
  (kill-new (bibtex-autokey-get-field "title"))
  (message "Copied Title: %s" (current-kill 0 t))
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
               (org-link-open-from-string (format "[[file:%s]]" optfile))))
      (if jg-bibtex-open-doi-with-pdf
          (+jg-bibtex-open-doi))
      (if jg-bibtex-open-url-with-pdf
          (+jg-bibtex-open-url))
      )))
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
          (goto-char (point-min))
          (search-forward (f-filename target)))
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
    (+jg-bibtex-refile-pdf current-prefix-arg)
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

(defun +jg-bibtex-get-files-fn (x)
  " Given a pair, return the cdr if car matches 'file' "
  (if (string-match "file" (car x))
      (string-trim (cdr x) "{" "}")))

(defun +jg-bibtex-refile-pdf (&optional destructive)
  " Refile a pdf from its location to its pdflib/year/author loc
returns the new location
"
  (if destructive
      (message "Destructive Refile"))
  (save-excursion
    (let* ((entry  (bibtex-parse-entry))
           (author (s-capitalize (bibtex-autokey-get-names)))
           (year   (bibtex-text-in-field "year"))
           (files  (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (pdflib jg-bibtex-pdf-loc)
           (finalpath (f-join pdflib year author))
           newlocs)
      (make-directory finalpath 'parents)

      (loop for file in files
            do
            (let* ((fname (f-filename file))
                   (target (f-join finalpath fname)))
              (message "Relocating %s to %s" file target)
              (if (s-equals? "y" (read-string (format "%sRefile to %s? " (if destructive "Destructive " "")
                                                      target)))
                  (progn (assert (not (f-exists? target)))
                         (if destructive (f-move file target)
                           (progn (f-copy file target)
                                  (f-move file (f-join (f-parent file) (format "_refiled_%s" fname)))))
                         (push target newlocs))
                (push file newlocs))
              )
            )

      ;; Update entry with new locations
      (loop for file in newlocs
            with count = 1
            do
            (bibtex-set-field (format "file%s" (if (eq count 1) "" count)) file)
            )
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
           (potential-completions (f-join jg-bibtex-loc-completions chosen))
           (source (if (f-exists? potential-completions)
                       (helm-build-in-file-source "Completion Helm"
                           potential-completions)))
           (dummy-action #'(lambda (x) (write-region (format "%s\n" (string-trim x)) nil potential-completions t) x))
           (dummy-source (helm-build-dummy-source "Completion Helm Dummy"
                           :action (helm-make-actions "Insert into file" dummy-action)))
           new-value
           )
      (setq new-value (if source
                          (helm :sources (list source dummy-source)
                                :buffer "*helm bibtex completions*"
                                :input curr-value
                                )
                          (read-string (format "(%s) New Value: " chosen))))
      (if new-value
          (bibtex-set-field chosen (string-trim new-value))
        )
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
(defun +jg-bibtex-toggle-doi-load ()
  (interactive)
  (setq jg-bibtex-open-doi-with-pdf (not jg-bibtex-open-doi-with-pdf))
  (message "Open DOI on pdf? %s" jg-bibtex-open-doi-with-pdf)
  )
(defun +jg-bibtex-toggle-url-load ()
  (interactive)
  (setq jg-bibtex-open-url-with-pdf (not jg-bibtex-open-url-with-pdf))
  (message "Open URL on pdf? %s" jg-bibtex-open-url-with-pdf)
  )


(defun +jg-bibtex-tweet-random-entry ()
  (interactive)
  (if (not bibtex-completion-bibliography)
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


(defun +jg-bibtex-dired-stub-entries ()
  " Discover all pdfs in a directory, create stubs for them "
  (interactive)
  (let* ((curr-dir (dired-current-directory))
         (files    (f-files curr-dir  (lambda (x) (or (f-ext? x "pdf")
                                                 (f-ext? x "epub")))))
         (target-bib (read-file-name "Todo-bib: " jg-bibtex-loc-bibtex))
         mentioned
         )

    ;; Get mentioned
    (with-temp-buffer
      (if (f-exists? target-bib)
          (insert-file target-bib))
      (goto-char (point-min))
      (while (re-search-forward "^\s*file[0-9]*\s*=\s*{\\(.+?\\)}" nil t)
        (pushnew (match-string 1) mentioned :test 'equal)
      )
      (goto-char (point-max))
      (insert "\n")
      (message "Found: %s\n Mentioned: %s\n Remaining: %s"
               (length files) (length mentioned) (length (-difference files
                                                                      mentioned)))
      (loop with count = 0
            for file in (-difference files mentioned)
            do
            (insert "@Misc{stub_" (int-to-string count) ",\n"
                    "  file = {"  file "}\n"
                    "}\n")
            (incf count)
            )
      (write-file target-bib)
      )
    )
  )

(defun +jg-bibtex-rename-file ()
  " Rename the file associated with the record "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((fields      (bibtex-parse-entry))
         (file-fields (-filter (lambda (x) (string-match "file" (car x))) fields))
         (field-selection (if (eq 1 (length file-fields))
                              (caar file-fields)
                            (read-string "File Select: " "file")))
         (filename (bibtex-autokey-get-field field-selection))
         (ext (f-ext filename))
         (base (f-parent filename))
         (new-name (read-string "New Filename: " (f-base filename)))
         (new-path (f-join base (format "%s.%s" new-name ext))))
    (message "Moving: %s\nto: %s" filename new-path)
    (f-move filename new-path)
    (bibtex-set-field field-selection new-path)
    )
  )
