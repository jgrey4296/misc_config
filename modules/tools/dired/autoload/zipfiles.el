;;; zipfiles.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;###autoload
(defun +jg-dired-async-list-zip-files ()
  "List the files contained in a zip file"
  (interactive)
  (let* ((marked (ensure-list (dired-get-marked-files)))
         (target-buffer (get-buffer-create "*Zip Files*"))
         )
    (with-current-buffer target-buffer
      (erase-buffer)
      (insert "\n--- Files Contained In Zip Archives:\n")
      )
    (make-process :name "zipfileslist"
                  :buffer target-buffer
                  :command (append (list "zipinfo") marked)
                  :sentinel (-partial '(lambda (targ p e) (when (not (process-live-p p))
                                                            (with-current-buffer targ (insert "\n---- Finished ----\n"))
                                                            (display-buffer targ)))
                                      target-buffer)
                  :noquery t
                  )
    )
)

;;;###autoload
(defun +jg-dired-extract-from-zip-file ()
  "Extract a selection of files from a zip"
  (interactive)
  (when-let* ((target (car-safe (dired-get-marked-files)))
              (is-zip (f-ext? target "zip"))
              (files (with-temp-buffer
                       (call-process "zipinfo" nil t nil "-1" target)
                       (split-string (buffer-string) "\n")
                       ))
              (action (-partial #'jg-dired-extract-zip-files target))
              (selected (ivy-read "Files to Extract: "
                                  files
                                  :require-match t
                                  :sort t
                                  :action action
                                  :multi-action action
                                  ))
              )
    )
  )

(defun jg-dired-extract-zip-files (archive fnames)
  (let ((files (ensure-list fnames)))
    (apply #'call-process "unzip" nil nil nil
           archive
           files
           )
    )
  )

;;; zipfiles.el ends here
