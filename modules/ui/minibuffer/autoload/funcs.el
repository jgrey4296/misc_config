;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-minibuffer-normal-or-exit ()
  (interactive)
  (if (not (evil-normal-state-p))
      (evil-normal-state)
    (abort-recursive-edit))
  )
