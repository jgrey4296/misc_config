;;; term/shell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-shell-new (&optional arg)
  "Use an existing shell, or with prefix arg create a new one.
If called from dired, will insert a cd to move to the dired buffer location
 "
  (interactive "p")
  (pcase arg
    ((and 1
          (guard (get-buffer "*shell*"))
          (guard (eq 'dired-mode (with-current-buffer (current-buffer) major-mode)))
          (let dir default-directory)
          )
     (shell)
     (with-current-buffer "*shell*"
       (let ((proc (get-buffer-process (current-buffer))))
         (process-send-string proc (format "cd %s; pwd" dir))
         (comint-send-input)
         )
       )
     )
    (1 (shell))
    (_ (shell (generate-new-buffer-name "*shell*")))
    )
  )

;;;###autoload
(defun +jg-term-switch ()
  (interactive)
  (let ((shells (-filter
                 (lambda (x)
                   (s-matches?
                    (rx string-start "*shell"
                        (0+ any)
                        string-end)
                    (buffer-name x)))
                 (buffer-list)))
        )
    (ivy-read "Shell: "
              (mapcar #'buffer-name shells)
              :action #'switch-to-buffer
              :caller 'jg-term-ivy-switch-term
              )
    )
  )

;;;###autoload
(defun +jg-shell-cd-prev ()
  "Call `cd -` and go back to where you came from "
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (process-send-string proc "cd -; pwd")
    (comint-send-input)
    )
  )
