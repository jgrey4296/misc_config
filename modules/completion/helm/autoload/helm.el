;;; helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-helm-next-source ()
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'source :direction 'next)
    )
  )
