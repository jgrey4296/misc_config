;;; helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-helm-next-source ()
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'source :direction 'next)
    )
  )

;;;###autoload
(defun +jg-helm-save-buffer ()
  (interactive)
  (let ((results (with-helm-buffer (buffer-string))))
    (helm-exit-and-execute-action
     #'(lambda (x)
         (with-temp-buffer-window "TestBuffer" 'display-buffer-pop-up-frame nil
           (princ results)
           )
         )
     )
    )
  )
