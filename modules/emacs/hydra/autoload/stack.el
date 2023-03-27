;;; +hydra-stack.el -*- lexical-binding: t; -*-

(defvar jg-hydra-stack nil)

;;;###autodef
(defun +jg-hydra-doc (var)
  (if var 1 0)
  )

;;;###autodef
(defun +jg-hydra-push (func)
  (push func jg-hydra-stack)
  )

;;;###autodef
(defun +jg-hydra-pop ()
  (interactive)
  (when jg-hydra-stack
    (funcall-interactively (pop jg-hydra-stack))
    )
  )
