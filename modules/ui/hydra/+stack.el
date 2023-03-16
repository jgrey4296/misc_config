;;; +hydra-stack.el -*- lexical-binding: t; -*-

(defvar jg-hydra-stack nil)

(defun +jg-hydra-doc (var)
  (if var 1 0)
  )

(defun +jg-hydra-push (func)
  (push func jg-hydra-stack)
  )

(defun +jg-hydra-pop ()
  (interactive)
  (when jg-hydra-stack
    (funcall-interactively (pop jg-hydra-stack))
    )
  )
