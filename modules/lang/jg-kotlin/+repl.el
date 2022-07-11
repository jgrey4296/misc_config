;;; +repl.el -*- lexical-binding: t; -*-

(defun +kotlin-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'kotlin-mode)
  (if (not (bufferp kotlin-repl-buffer))
      (kotlin-repl))
  (get-buffer-create kotlin-repl-buffer)
  )

(set-repl-handler! 'kotlin-mode '+kotlin-mode/open-repl)
