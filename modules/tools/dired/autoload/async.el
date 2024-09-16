;;; delete.el -*- lexical-binding: t; -*-

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
(defun +jg-dired-hash-files ()
  (interactive)
  (let* ((marked (ensure-list (dired-get-marked-files)))
         (target-buffer (get-buffer-create "*file-hashes*"))
         )
    (with-current-buffer target-buffer
      (insert "\n--- File Hashes:\n")
      )
    (make-process :name "file-hash"
                  :buffer target-buffer
                  :command (append '("md5sum") marked)
                  :sentinel (-partial '(lambda (targ p e) (when (string-equal "finished\n" e)
                                                             (display-buffer targ)))
                                       target-buffer)
                  :noquery t
                  )
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
  (let ((buffer (get-buffer-create "*PyServer*")))
    (make-process :name "py-server"
                  :buffer buffer
                  :command (list "python" "-m" "http.server"
                                 "--directory" default-directory
                                 "--bind" "127.0.0.1"
                                 "8000")
                  :noquery t
                  )
    (librarian-browser--open-url "http://127.0.0.1:8000")
    )
  )
