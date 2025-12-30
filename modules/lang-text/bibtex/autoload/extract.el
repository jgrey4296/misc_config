;;; extract.el -*- lexical-binding: t; no-byte-compile: t; -*-


(defun +jg-bibtex-extract-pages ()
  (interactive)
  (bibtex-beginning-of-entry)
  (save-excursion
    (let* ((pages (bibtex-autokey-get-field '("pdf_pages" "pages")))
           (key (alist-get "=key=" (bibtex-parse-entry) nil nil 'equal))
           (file (bibtex-autokey-get-field "file"))
           (filename (f-filename file))
           (temp-target (f-join librarian--biblio-edit-todo-files-loc
                                (format "%s.pdf" key)))
           )
      (when (or (string-empty-p pages) (null pages))
        (user-error "No Pages defined for: %s" key))

      (unless (string-match "[0-9]+-[0-9]+" pages)
        (user-error "Pages is not in the format of [0-9]+-[0-9]+: %s" pages))

      (unless file
        (user-error "Entry has no associated file: %s" key))

      (unless (f-ext? file "pdf")
        (user-error "File is not a pdf: %s" file))

      (when (f-exists? temp-target)
        (f-delete temp-target))

      (message "Extracting pages: %s of %s" pages filename)

      ;; Call pdftk
      (call-process "pdftk" nil nil nil
                    file "cat" pages "output" temp-target
                    )

      ;; Set the new file
      (bibtex-set-field "file" temp-target)

      (message "Refiling")
      (librarian--biblio-refile-pdf)
      )
    )
  )


;;; extract.el ends here
;; pdftk ./aisb1974.pdf cat 4-8 output ./test.pdf
