;;; +repl.el -*- lexical-binding: t; -*-

(defun +ceptre-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'ceptre)
  (if (not (bufferp ceptre-repl-buffer-name))
      (run-ceptre 0))
  (get-buffer-create ceptre-repl-buffer-name)
  )

(set-repl-handler! 'ceptre-mode '+ceptre-mode/open-repl)



(defun +soar-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'soar)
  (if (not (bufferp soar-comint-buffer-name))
      (run-soar-comint))
  (get-buffer-create soar-comint-buffer-name)
  )

(set-repl-handler! 'soar-mode '+soar-mode/open-repl)
