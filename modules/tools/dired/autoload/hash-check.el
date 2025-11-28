;;; hash-check.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar jg-hash-check-command "shasum %s | sort | guniq -w 40 | awk '{print $2}'")
(defvar jg-hash-check-buffer "*shasum*")


;;;###autoload
(defun +jg-dired-hash-duplicates()
  " get the hashes of the marked files, and unmark all that aren't duplicates "
  (interactive)
  (let* ((marked (dired-get-marked-files))
         (quoted (mapcar #'shell-quote-argument marked))
         uniqs
         dups
         )
    (with-current-buffer (get-buffer-create jg-hash-check-buffer) (erase-buffer))
    (shell-command (format jg-hash-check-command (s-join " " quoted)) jg-hash-check-buffer)
    (setq uniqs (split-string (with-current-buffer jg-hash-check-buffer (buffer-string)) "\n" " ")
          dups (-difference marked uniqs))
    (dired-unmark-all-marks)
    (dired-mark-if (and (dired-get-filename nil t) (-contains? dups (dired-get-filename nil t))) "Duplicated SHA Checksum")
    (message "%s Duplicates" (length dups))
    )
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
;;; hash-check.el ends here
