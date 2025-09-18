;;; repls.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +erlang/open-repl (&optional arg)
  (interactive "P")
  (require 'erlang)
  (if (not (bufferp inferior-erlang-buffer-name))
      (run-erlang))
  (get-buffer-create inferior-erlang-buffer-name)
  )
