;;; util/debug/+funcs.el -*- lexical-binding: t; -*-

(defun debug-package-load (name)
  (require 'loadhist)
  (message "Package %s used in: %s" name (file-dependents (feature-file (intern name))))
  )
