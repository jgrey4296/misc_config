;;; editor/window-control/+funcs.el -*- lexical-binding: t; -*-

(defun +window-ring-block-reset (arg)
  (interactive "p")
  (window-ring-setup-columns arg t)
  )
