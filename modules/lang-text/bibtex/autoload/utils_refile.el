;;; +refile.el -*- lexical-binding: t; -*-
(require 'bibtex)
(require 's)
(require 'f)

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun +jg-bibtex-refile-to-unsourced ()
  " Kill the current entry and insert it in the appropriate year's bibtex file "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((target jg-bibtex-unsourced-bib-file)
         (response (read-string "Refile to unsourced? "))
         )
    (unless (f-exists? target) (f-touch target))
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
