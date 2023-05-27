;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +indent-guides-disable-maybe-h ()
  (and highlight-indent-guides-mode
       (bound-and-true-p org-indent-mode)
       (highlight-indent-guides-mode -1))
  )

;;;###autoload
(defun font-lock-fontify-region (beg end &optional loudly)
  (funcall font-lock-fontify-region (max beg 1) end loudly)
  )
