;;; +bib-management.el -*- lexical-binding: t; -*-
(require 'bibtex-completion)

;;;###autoload
(defun +jg-bibtex-build-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (interactive)
  (setq bibtex-completion-bibliography (directory-files jg-bibtex-loc-bibtex 't "\.bib$")
        jg-bibtex-helm-candidates nil
        )
  (bibtex-completion-clear-cache)
  (bibtex-completion-init)
  (mapcar #'+jg-bibtex-process-candidates (bibtex-completion-candidates))
  )

;;;###autoload
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

;;;###autoload
(defun +jg-bibtex-file-expand (x)
  (+jg-bibtex-get-files-fn (cons "file" x))
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
