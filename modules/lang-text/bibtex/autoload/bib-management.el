;;; +bib-management.el -*- lexical-binding: t; -*-
(require 'bibtex-completion)


;;;###autoload
(defun +jg-bibtex-file-expand (x)
  (librarian--biblio-get-files-fn (cons "file" x))
  )

;;;###autoload
(defun +jg-bibtex-suppress-watchers ()
  "bibtex-completion-init adds file watchers for all bibtex files
This can be annoying at times.
This function toggles clearing those watchers and recreating them later
"
  (interactive)
  (when bibtex-completion-file-watch-descriptors
      (progn (message "Clearing Bibtex Watchers")
             (message "Descriptors: %s" (length (hash-table-keys file-notify-descriptors)))
             (mapc (lambda (watch-descriptor)
                     (file-notify-rm-watch watch-descriptor))
                   bibtex-completion-file-watch-descriptors)
             (message "Post Removal Descriptors: %s" (length (hash-table-keys file-notify-descriptors)))
             (setq bibtex-completion-file-watch-descriptors nil))
    )
  nil
  )

;;;###autoload
(defun +jg-bibtex-init-file-watchers ()
  " Check that all specified bibliography files exist and add file
   watches for automatic reloading of the bibliography when a file
   is changed: "
  (interactive)
  (when bibtex-completion-file-watch-descriptors
    (error "There are already file watchers for bibtex files"))
  (setq bibtex-completion-file-watch-descriptors
        (cl-loop for file in (bibtex-completion-normalize-bibliography)
                 if (f-file? file)
                 collect
                 (file-notify-add-watch file
                                        '(change)
                                        (lambda (event) (bibtex-completion-candidates)))
                 )
        )
  )
