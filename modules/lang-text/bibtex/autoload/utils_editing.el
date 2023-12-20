;;; +editing.el -*- lexical-binding: t; -*-
(require 'bibtex)

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun +jg-bibtex-copy-key ()
  " Copy the cite key of the entry under point "
  (interactive)
  (kill-new (bibtex-completion-get-key-bibtex))
  (message "Copied Key: %s" (current-kill 0 t))
  )

;;;###autoload
(defun +jg-bibtex-copy-title ()
  (interactive)
  (kill-new (bibtex-autokey-get-field "title"))
  (message "Copied Title: %s" (current-kill 0 t))
  )

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun +jg-bibtex-clean-error-move-toggle ()
  (interactive)
  (setq jg-bibtex-clean-move-entry-on-fail (not jg-bibtex-clean-move-entry-on-fail))
  (message "Error on clean entry %s move to end of file" (if jg-bibtex-clean-move-entry-on-fail
                                                             "will"
                                                           "will not"))
  )

;;;###autoload
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
         (new-name (string-trim (read-string "New Filename: " (f-base filename))))
         (new-path (f-join base (format "%s.%s" new-name ext))))
    (message "Moving: %s\nto: %s" filename new-path)
    (f-move filename new-path)
    (bibtex-set-field field-selection new-path)
    )
  )

;;;###autoload
(defun +jg-bibtex-dired-unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (seq-each '+jg-bibtex-unify-pdf-locations-in-file (dired-get-marked-files))
  )

;;;###autoload
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

;;;###autoload
(defun +jg-bibtex-swap-editor-author ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (save-excursion (bibtex-parse-entry)))
           (editor (alist-get "editor" entry nil nil 'equal))
           (author (alist-get "author" entry nil nil 'equal))
           )
      (cond ((and editor author) nil)
            (editor
             (bibtex-set-field "author" (substring editor 1 -1))
             (bibtex-beginning-of-entry)
             (if (re-search-forward "editor" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            (author
             (bibtex-set-field "editor" (substring author 1 -1))
             (bibtex-beginning-of-entry)
             (when (re-search-forward "author" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            )
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-swap-booktitle-journal ()
  (interactive)
    (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (save-excursion (bibtex-parse-entry)))
           (book (alist-get "booktitle" entry nil nil 'equal))
           (journal (alist-get "journal" entry nil nil 'equal))
           )
      (cond ((and book journal) nil)
            (book
             (bibtex-set-field "journal" (substring book 1 -1))
             (bibtex-beginning-of-entry)
             (if (re-search-forward "booktitle" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            (journal
             (bibtex-set-field "booktitle" (substring journal 1 -1))
             (bibtex-beginning-of-entry)
             (when (re-search-forward "journal" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            )
      )
    )
  )
