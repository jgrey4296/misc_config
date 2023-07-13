;;; auto-modes.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-default-debug-auto-mode ()
  (interactive)
  (let* ((name (file-name-sans-versions (or (buffer-file-name) (buffer-name))))
         (result (assoc-default name auto-mode-alist 'string-match))
        )
    (message "Buffer %s (%s) Matches: %s" (buffer-name) major-mode result)
    )
  )
