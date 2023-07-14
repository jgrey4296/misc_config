;;; +repl.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +acab-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'acab-ide)
  (if (not (bufferp acab-comint/buffer-name))
      (acab-comint/init-repl))
  (get-buffer-create acab-comint/buffer-name)
  )
