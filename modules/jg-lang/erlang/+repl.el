;;; +repl.el -*- lexical-binding: t; -*-


(defun +erlang/open-repl (&optional arg)
  (interactive "P")
  (require 'erlang)
  (if (not (bufferp erlang-shell-buffer-name))
      (run-erlang))
  (get-buffer-create erlang-shell-buffer-name)
  )

(set-repl-handler! 'erlang-mode '+erlang/open-repl)
