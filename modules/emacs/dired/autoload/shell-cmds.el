;; shell-cmds.el -*- lexical-binding: t; -*-

(defvar jg-dired-seq-procs nil)

(defun jg-dired--seq-fn (cmdlist file)
  (mapcar #'(lambda (x)
              (cond ((s-contains? "??" x)
                     (f-no-ext file))
                    ((string-equal x "?")
                     file)
                    (t x)))
          cmdlist))

(defun jg-dired--seq-sentinel (cmd-builder files proc state)
  (setq jg-dired-seq-procs (-filter #'process-live-p jg-dired-seq-procs))
  (cond ((and files (not (process-live-p proc)))
         (message "Starting Process for next file: %s" (car files))
         (push (make-process :name "jg-dired-seq-cmd"
                             :buffer nil
                             :command (funcall cmd-builder (pop files))
                             :noquery t
                             :sentinel (-partial #'jg-dired--seq-sentinel cmd-builder files)
                             )
               jg-dired-seq-procs)
         )
        (t (message "Seq Proc: %s %s" state files))
        )
  )

(defun +jg-dired-seq-command ()
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg nil nil t))
         (cmd (split-string-shell-command (read-string "Sequential Cmd (? for file, ?? for extensionless file): ")))
         (cmd-builder (-partial #'jg-dired--seq-fn cmd))
         )
    (message "Example Cmd: %s" (funcall cmd-builder (car files)))
    (push (make-process :name "jg-dired-seq-cmd"
                        :buffer nil
                        :command (funcall cmd-builder (pop files))
                        :noquery t
                        :sentinel (-partial #'jg-dired--seq-sentinel cmd-builder files)
                        )
          jg-dired-seq-procs)
    )
  )
