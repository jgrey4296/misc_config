;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ivy-popup-messages (&optional arg)
  (interactive "P")
  (+jg-popup-ivy-open messages-buffer-name)
  (with-current-buffer messages-buffer-name
    (when current-prefix-arg
      (+jg-text-clear-buffer))
    )
  (with-selected-window (get-buffer-window messages-buffer-name)
    (goto-char (point-max))
    (recenter -1)
    )
  )

;;;###autoload
(defun +jg-ivy-file-predicate (x)
  (and (s-matches? jg-ivy-file-regexp x)
       (not (s-matches? jg-ivy-file-reject-regexp x)))
  )
