;; bibtex

;;-- utils
(defun +jg-bibtex-build-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (setq bibtex-completion-bibliography (directory-files jg-bibtex-loc-bibtex 't "\.bib$")
        jg-bibtex-helm-candidates nil
        )
  )
(defun +jg-bibtex-get-files-fn (x)
  " Given a pair, return the cdr if car matches 'file' "
  (when-let ((isfile (string-match "^file[[:digit:]]*" (car x)))
             (text (string-trim (cdr x) "{" "}"))
             )
    (expand-file-name (if (f-relative? text)
                          (f-join jg-bibtex-pdf-loc text)
                        text))
        )
    )
(defun +jg-bibtex-file-expand (x)
  (+jg-bibtex-get-files-fn (cons "file" x))
  )
(defun +jg-bibtex-suppress-watchers ()
  "bibtex-completion-init adds file watchers for all bibtex files
This can be annoying at times.
This function toggles clearing those watchers and recreating them later
"
  (interactive)
  (if bibtex-completion-file-watch-descriptors
      (progn (message "Clearing Bibtex Watchers")
        (mapc (lambda (watch-descriptor)
                     (file-notify-rm-watch watch-descriptor))
                   bibtex-completion-file-watch-descriptors)
             (setq bibtex-completion-file-watch-descriptors nil))
    (progn (message "Setting Bibtex Watchers")
      (mapc
       (lambda (file)
         (if (f-file? file)
             (let ((watch-descriptor (file-notify-add-watch file '(change)
                                                            (lambda (event)
                                                              (bibtex-completion-candidates)))))
               (setq bibtex-completion-file-watch-descriptors
                     (cons watch-descriptor bibtex-completion-file-watch-descriptors)))
           (user-error "Bibliography file %s could not be found" file)))
       (bibtex-completion-normalize-bibliography))))
  )
;;-- end utils

;;-- file opening
(defun +jg-bibtex-open-pdf (&optional path)
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (let* ((target (if path
                       path
                     (bibtex-autokey-get-field '("file" "OPTfile"))))
           )
      (message "%s" target)
      (cond ((string-empty-p target)
             (message "No File to open"))
            ((not (file-exists-p target))
             (message "File does not exist"))
            ((f-ext? target "epub")
             (shell-command (concat jg-bibtex-open-epub-cmd (shell-quote-argument target))))
            ((f-ext? target "pdf")
             (shell-command (concat jg-bibtex-open-pdf-cmd (shell-quote-argument target))))
            (t
             (message "Unrecognized type")
             (shell-command (concat "open " (shell-quote-argument target))))
            )

      (when jg-bibtex-open-doi-with-pdf
          (+jg-bibtex-open-doi))
      (when jg-bibtex-open-url-with-pdf
          (+jg-bibtex-open-url))
      )))
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
(defun +jg-bibtex-find-folder ()
  " Find the fold in which the entry's associated file exists "
  (interactive)
  (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
    (when (and (not (string-empty-p target)) (f-exists? (f-parent target)))
      (message "Opening %s" (f-parent target))
      (find-file-other-window (f-parent target))
      (goto-char (point-min))
      (search-forward (f-filename target)))
    )
  )
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
(defun +jg-bibtex-quicklook-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
      (message "%s : %s" target (file-exists-p target))
      (async-shell-command (concat "qlmanage -p "
                                   (shell-quote-argument target)
                                   " 2>/dev/null"))
      )))
;;-- end file opening

;;-- entry editing
(defun +jg-bibtex-edit-entry-type ()
  " Edit the @type of a bibtex entry, using
bibtex-BibTeX-entry-alist for completion options "
  (interactive)
  (let* ((type-options (mapcar 'car bibtex-entry-alist))
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
(defun +jg-bibtex-copy-field ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (fields (-reject #'(lambda (x) (s-contains? "=" x)) (mapcar 'car entry)))
           (selected (ivy-read "Field to Copy: " fields)))
      (kill-new (bibtex-autokey-get-field selected)))
    )
  )
(defun +jg-bibtex-quickswap ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((journal (bibtex-autokey-get-field "journal"))
           (booktitle (bibtex-autokey-get-field "booktitle")))
      (bibtex-set-field "booktitle" journal)
      (bibtex-set-field "journal" booktitle)
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
(defun +jg-bibtex-dired-unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (seq-each '+jg-bibtex-unify-pdf-locations-in-file (dired-get-marked-files))
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
      (when (eq 6 (length (match-data)))
        (replace-match jg-bibtex-pdf-replace-library-string t nil nil 2))
      )
    (write-file name)
    )
  )
;;-- end entry editing

;;-- refiling
(defun +jg-bibtex-refile-pdf (&optional destructive)
  " Refile a pdf from its location to its pdflib/year/author loc
returns the new location
"
  (when destructive
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

      (cl-loop for file in files
            do
            (let* ((fname (f-filename file))
                   (target (f-join finalpath fname))
                   )
              (message "Relocating %s to %s" file target)
              (if (s-equals? "y" (read-string (format "%sRefile to %s? " (if destructive "Destructive " "") target)))
                  (progn (cl-assert (not (f-exists? target)))
                         (if destructive
                             (f-move file target)
                           (progn (f-copy file target)
                                  (f-move file (f-join (f-parent file) (format "_refiled_%s" fname)))))
                         (push target newlocs))
                (push file newlocs))
              )
            )

      ;; Update entry with new locations
      (cl-loop for file in newlocs
            with count = 1
            do
            (bibtex-set-field (format "file%s" (if (eq count 1) "" count)) file)
            (cl-incf count)
            )
      )
    )
  )
(defun +jg-bibtex-refile-by-year ()
  " Kill the current entry and insert it in the appropriate year's bibtex file "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((year (bibtex-text-in-field "year"))
         (year-file (format "%s.bib" year))
         (response (when year (read-string (format "Refile to %s? " year-file))))
         (target (if (or (s-equals? "y" response) (string-empty-p response))
                     (f-join jg-bibtex-loc-bibtex year-file)
                   (completing-read "Bibtex file: "
                                    (f-entries bib-path
                                               (lambda (f) (f-ext? f "bib"))))))
         )
    (unless (f-exists? target)
      (f-touch target)
      )
    (+jg-bibtex-refile-pdf current-prefix-arg)
    (bibtex-kill-entry)
    (with-temp-buffer
      (insert-file-contents target)
      (goto-char (point-max))
      (insert "\n")
      (bibtex-yank)
      (write-file target nil)
      )
    )
  )
;;-- end refiling

;;-- ui
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
         (search-string (format (if arg
                                    jg-bibtex-dblp-search-string
                                  jg-bibtex-scholar-search-string)
                                cleaned))
         )
    (browse-url search-string)
    )
  )
;;-- end ui

;;-- entry stubbing
(defun +jg-bibtex-dired-stub-entries ()
  " Discover all pdfs in a directory, create stubs for them "
  (interactive)
  (let* ((curr-dir (dired-current-directory))
         (files    (f-files curr-dir  (lambda (x) (or (f-ext? x "pdf") (f-ext? x "epub")))))
         (target-bib (read-file-name "Todo-bib: " jg-bibtex-loc-bibtex))
         mentioned
         )
    ;; Get mentioned
    (unless (f-exists? target-bib)
      (error "Target Stub File doesn't exist: %s" target-bib))
    (with-temp-buffer
      (insert-file-contents target-bib))
    (goto-char (point-min))
    (while (re-search-forward "^\s*file[0-9]*\s*=\s*{\\(.+?\\)}" nil t)
      (cl-pushnew (match-string 1) mentioned :test 'equal)
      )
    (goto-char (point-max))
    (insert "\n")
    (message "Found: %s\n Mentioned: %s\n Remaining: %s"
             (length files) (length mentioned) (length (-difference files
                                                                    mentioned)))
    (cl-loop with count = 0
          for file in (-difference files mentioned)
          do
          (insert (format "@Misc{stub_%s,\n" (int-to-string count))
                  (format "  year = {%s},\n" (nth 2 (calendar-current-date)))
                  (format "  title = {%s},\n" (f-no-ext (f-filename file)))
                  (format "  file = {%s},\n"  file)
                  "}\n")
          (cl-incf count)
          )
    (write-file target-bib)
    )
  )

;;-- end entry stubbing

;;-- meta retrieval
(defun +jg-bibtex-meta-retrieval ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (files (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (result (cl-loop for file in files
                            collect
                            (shell-command-to-string (format "ebook-meta %s" (shell-quote-argument (expand-file-name (if (f-relative? file) (f-join jg-bibtex-pdf-loc file) file)))))
                            ))
           )
      (with-temp-buffer-window "*Metadata*" 'display-buffer-pop-up-window nil
        (princ (s-join "\n" result))
        )
      )
    )
  )

(defun +jg-bibtex-apply-meta ()
  " Use Calibre's ebook-meta program to take bibtex data and apply it to a pdf or epub "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (target (shell-quote-argument (bibtex-autokey-get-field "file")))
           (keys (mapcar #'car entry))
           (meta-opts '(("title     " . "-t")
                        ("author    " . "-a")
                        ("comments  " . "-c")
                        ("publisher " . "-p")
                        ("series    " . "-s")
                        ("number    " . "-i")
                        ("rating    " . "-r")
                        ("date      " . "-d")
                        ("isbn      " . "--isbn")
                        ("ident     " . "--identifier=")
                        ("tags      " . "--tags=")
                        ("category  " . "--category=")
                        ("*Apply*")
                        ))
          current
          )
      ;; read fields and wrap
      (while (not (s-equals? "*Apply*" (setq current (ivy-read "Assign to option: " meta-opts))))
        (when (alist-get current meta-opts nil nil #'equal)
          (push (cons  (alist-get current meta-opts nil nil #'equal)
                       (let* ((field (ivy-read "Select Field: " keys))
                              (text (bibtex-autokey-get-field field)))
                         (if (string-empty-p text) field text)))
                arg-pairs)
          )
        )
      (unless arg-pairs
        (error "Nothing to do"))
      ;; build and call
      (let* ((options (s-join " " (cl-loop for pair in arg-pairs
                                          collect
                                          (format (if (s-matches? "=$" (car pair)) "%s\"%s\"" "%s \"%s\"") (car pair) (cdr pair)))))
             (command (format "ebook-meta %s %s" target options))
             )
        (message "Command: %s" command)
        (shell-command command)
        )
      )
    )
  )

(defun +jg-bibtex-set-ebook-cover ()
  " Use Calibre's ebook-meta program to select an image and apply it as an epub's cover image "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (target (shell-quote-argument (bibtex-autokey-get-field "file")))


           )
      )
    )
  )
;;-- end meta retrieval

;;-- window-switching
(defun +jg-bibtex-window-set-downloads ()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind
      (bookmark-jump "downloads")
      )
    (with-selected-window bot-wind
      (bookmark-jump "in_progress_pdfs")
      )
    )
  )
(defun +jg-bibtex-window-set-dropbox()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind
      (bookmark-jump "dropbox")
      )
    (with-selected-window bot-wind
      (bookmark-jump "in_progress_pdfs")
      )
    )
  )

;;-- end window-switching
