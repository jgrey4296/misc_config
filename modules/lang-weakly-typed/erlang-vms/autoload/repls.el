;;; repls.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +erlang/open-repl (&optional arg)
  (interactive "P")
  (require 'erlang)
  (if (not (bufferp inferior-erlang-buffer-name))
      (run-erlang))
  (get-buffer-create inferior-erlang-buffer-name)
  )

;;;###autoload
(defun +elixir/open-repl (&optional arg)
  (interactive "P")
  (require 'alchemist-iex)
  (if (not (bufferp elixir-shell-buffer-name))
      (alchemist-iex-run))
  (get-buffer-create elixir-shell-buffer-name)
  )
