;;; editor/rotate-text/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(cl-defun set-rotate-patterns! (modes &key symbols words patterns)
  "Declare :symbols, :words or :patterns (all lists of strings) that
`rotate-text' will cycle through."
  (signal 'deprecated modes))
