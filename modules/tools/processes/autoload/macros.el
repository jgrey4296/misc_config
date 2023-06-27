;;; macros.el -*- lexical-binding: t; -*-

;;;###autoload
(defun jg-process-sentinel! (target proc status)
  (unless (process-live-p proc)
     (let ((text (with-current-buffer (process-buffer proc)
                   (buffer-string))))
       (with-current-buffer target 'display-buffer-pop-up-window nil
                            (goto-char (point-min))
                            (insert (format "--- Proc: %s\n" (process-name proc)))
                            (insert text)
                            (insert (format "\n--- End Proc: %s.\n" (process-name proc)))
                            )
       )
     ;; (with-current-buffer (process-buffer proc)
     ;;   (erase-buffer))
     (kill-buffer (process-buffer proc))
     )
  )

;;;###autoload
(defmacro with-process-wrap! (buffer &rest body)
  (let ((procs (make-symbol "tempprocs"))
        (sent (make-symbol "sentinel"))
        )
    `(progn
       (with-current-buffer (get-buffer-create ,buffer)
         (erase-buffer))
       (let ((,procs ,@body)
             (,sent (-partial #'jg-process-sentinel! ,buffer))
             )
         (cond ((listp ,procs)
                (message "Applying sentinel to processes %s" ,procs)
                (mapc (-rpartial #'set-process-sentinel ,sent) ,procs))
               (t
                (message "Applying single sentinel %s" ,procs)
                (set-process-sentinel ,procs ,sent))
               )
         (save-selected-window
           (display-buffer ,buffer 'display-buffer-pop-up-window))
         )
       )
    )
  )
