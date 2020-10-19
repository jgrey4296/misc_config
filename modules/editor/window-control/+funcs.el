;;; editor/window-control/+funcs.el -*- lexical-binding: t; -*-

(defun +window-ring-block-reset (arg)
  (interactive "p")
  (window-ring-minor-mode 1)
  (window-ring-setup-columns arg t)
  )
