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
  (when (and (string-equal event "finished\n")
             (not (string-empty-p (with-current-buffer buffer) (s-trim (buffer-string)))))
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
