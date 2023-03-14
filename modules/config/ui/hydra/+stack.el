;;; +hydra-stack.el -*- lexical-binding: t; -*-

(defvar jg-ui-toggle-hydra-stack nil)

(defun +jghdoc (var)
  (if var 1 0)
  )
(defun +jgh-push (func)
  (push func jg-ui-toggle-hydra-stack)
  )
(defun +jgh-pop ()
  (interactive)
  (when jg-ui-toggle-hydra-stack
    (funcall-interactively (pop jg-ui-toggle-hydra-stack))
    )
  )
