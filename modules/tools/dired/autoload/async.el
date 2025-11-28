;;; async.el -*- lexical-binding: t; -*-
(defvar jg-dired-du-cmd "du")
(defvar jg-dired-du-args '("-hsc"))

(defun +jg-dired-async-delete-sentinel (buffer out-buffer process event)
  (when (string-equal "finished\n" event)
    (with-current-buffer buffer
        (revert-buffer)
        )
    (with-current-buffer out-buffer
        (insert "\n--- Trashing Finished\n")
        )
    (message "Trash Finished")
    )
  )

(defun +jg-dired-du-sentinel (buffer process event)
  (when (string-equal event "finished\n")
    (with-current-buffer buffer
      (message "Dir Size: %s" (s-trim (buffer-string)))
      )
    )
  )

(defun +jg-dired-cookiecutter-sentinel (buffer process event)
  (when (not (process-live-p process))
    (+popup-buffer buffer)
    )
  )

;;;###autoload
(defun +jg-dired-async-trash (&optional file)
  (interactive)
  (let* ((marked (ensure-list (or file (dired-get-marked-files))))
         (t-files  (-filter #'f-file? marked))
         (t-dirs   (-filter #'f-directory? marked))
         (buffer (current-buffer))
         (target-buffer (get-buffer-create "*trash-async*"))
         )
    (with-current-buffer target-buffer
      (insert "\n--- Trashing Started")
      (insert "\n--- Files: " (string-join (mapcar #'f-filename t-files) " .. "))
      (insert "\n--- Dirs: " (string-join (mapcar #'f-filename t-dirs) " .. "))
      )
    (pcase (read-answer (format "Trash Files? %s\n   and Dirs? %s\n"
                                (string-join (mapcar #'f-filename t-files) " .. ")
                                (string-join (mapcar #'f-filename t-dirs) " .. ")
                                )
                     '(("yes" ?y)
                       ("no"  ?n)))
      ("yes"
       (make-process :name "trash-async"
                     :buffer target-buffer
                     :command (append '("trash" "-v") marked)
                     :sentinel (-partial '+jg-dired-async-delete-sentinel buffer target-buffer)
                     )
       (message "Trashing Started")
       )
      ("no"
       (message "Cancelling Trash")
       )
      )

    )
  )

;;;###autoload
(defun +jg-dired-dir-size ()
  (interactive)
  (let ((marked (dired-get-marked-files))
        (buffer (get-buffer-create "*du-async*"))
        )
    (with-current-buffer buffer (erase-buffer))
    (make-process :name "du-async"
                  :buffer buffer
                  :command (append (list jg-dired-du-cmd) jg-dired-du-args marked)
                  :sentinel (-partial '+jg-dired-du-sentinel buffer)
                  )
    )
  )

;;;###autoload
(defun +jg-dired-quick-look ()
  (interactive)
  (async-shell-command (format "qlmanage -p %s 2>/dev/null"
                               (dired-get-filename)))
  )

;;;###autoload
(defun +jg-dired-cookiecutter ()
  (interactive)
  (let* ((templates (mapcar #'f-base (f-directories jg-snippets-project-templates-dir)))
         (chosen (ivy-read "Project Template: " templates :require-match t))
         (name (format "proj_name=%s" (read-string "Project Name: ")))
         (buffer (get-buffer-create "*cookiecutter-async*"))
        )
    (with-current-buffer buffer (erase-buffer))

    (make-process :name "cookiecutter-async"
                  :buffer buffer
                  :command (list "cookiecutter" "--no-input" chosen name)
                  :sentinel (-partial '+jg-dired-cookiecutter-sentinel buffer)
                  )
    )
  )

;;;###autoload
(defun +jg-list-trash ()
  (interactive)
  (shell-command "trash-list")
  )

;;;###autoload
(defun +jg-dired-tesseract ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (unless (eq 1 (length files))
      (user-error "Just run on one file"))
    (shell-command (format "tesseract %s quote" (shell-quote-argument (car files))))
    (find-file "quote.txt")
    )
  )


;;;###autoload
(defun +jg-dired-scan-files ()
  (interactive)
  (let* ((marked (ensure-list (dired-get-marked-files)))
         (target-buffer (get-buffer-create "*File Scans*"))
         )
    (with-current-buffer target-buffer
      (erase-buffer)
      (insert "\n--- File Scans:\n")
      )
    (make-process :name "filescan"
                  :buffer target-buffer
                  :command (append (list "clamscan" "--recursive" "--stdout" "-l" (expand-file-name "logs/clamscan.log" user-cache-dir)) marked)
                  :sentinel (-partial '(lambda (targ p e) (when (not (process-live-p p))
                                                            (with-current-buffer targ (insert "\n---- Finished ----\n"))
                                                            (display-buffer targ)))
                                       target-buffer)
                  :noquery t
                  )
    )
  )

;;;###autoload
(defun +jg-dired-update-clamscan ()
  (interactive)
  (let ((target (get-buffer-create "*Clamscan Update*")))
    (make-process :name "update-clamscan"
                  :buffer target
                  :command (list "freshclam" "-l" (expand-file-name "logs/freshclam.log" user-cache-dir))
                  :sentinel (-partial '(lambda (targ p e) (when (not (process-live-p p))
                                                            (with-current-buffer targ (insert "\n---- Finished ----\n"))
                                                            (display-buffer targ)))
                                      target)
                  :noquery t
                  )
    )
  )

(defconst jg-dired-full-meta-args (list
                                    "-g"                    ;; print group headings
                                    "-a"                    ;; allow duplicates
                                    "-u"                    ;; extract unknown tags
                                    ))

;;;###autoload
(defun +jg-dired-exiftool-files ()
  (interactive)
  (let ((marked (ensure-list (dired-get-marked-files)))
        (target (get-buffer-create "*File Metadata*")))
    (with-current-buffer target
      (erase-buffer)
      )
    (make-process :name "get-metadata"
                  :buffer target
                  :command (append (list "exiftool") jg-dired-full-meta-args marked)
                  :sentinel (-partial '(lambda (targ p e) (when (not (process-live-p p))
                                                            (display-buffer targ)
                                                            (with-selected-window (get-buffer-window targ)
                                                              (recenter -1))))
                                      target)
                  :noquery t
                  )
    )

  )

;;;###autoload
(defun +jg-dired-async-server ()
  (interactive)
  (let ((buffer (get-buffer-create "*PyServer*"))
        (location default-directory)
        ;; (location (read-directory-name "Start Server in: " default-directory))
        (index (read-directory-name "Index: " default-directory))
        )
    (make-process :name "py-server"
                  :buffer buffer
                  :command (list "python" "-m" "http.server"
                                 "--directory" location
                                 "--bind" "127.0.0.1"
                                 "8000")
                  :noquery t
                  )
    (librarian-browse-open
     (format "http://127.0.0.1:8000/%s" (if (equal index default-directory)
                                            ""
                                          (f-base index)))
     )
    )
  )

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
