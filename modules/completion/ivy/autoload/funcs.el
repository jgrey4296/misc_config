;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ivy-popup-messages (&optional arg)
  (interactive "P")
  (with-current-buffer "*Messages*"
    (when current-prefix-arg
      (+jg-text-clear-buffer))
    (goto-char (point-max))
    )
  (+jg-popup-ivy-open "*Messages*")
  )
