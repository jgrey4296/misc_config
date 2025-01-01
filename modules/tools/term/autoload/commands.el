;;; term/shell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-shell-new (&optional arg)
  "Use an existing shell, or with prefix arg create a new one "
  (interactive "p")
  (pcase arg
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
