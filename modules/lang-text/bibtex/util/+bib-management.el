;;; +bib-management.el -*- lexical-binding: t; -*-

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
