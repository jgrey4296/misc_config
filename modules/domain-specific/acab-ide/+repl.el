;;; +repl.el -*- lexical-binding: t; -*-

(defun +acab-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'acab-ide)
  (if (not (bufferp acab-comint/buffer-name))
      (acab-comint/init))
  (get-buffer-create acab-comint/buffer-name)
  )

(set-repl-handler! 'acab-rule-mode 'acab-mode/open-repl)
