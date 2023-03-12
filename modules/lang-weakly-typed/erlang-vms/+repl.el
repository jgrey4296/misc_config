;;; +repl.el -*- lexical-binding: t; -*-


(defun +erlang/open-repl (&optional arg)
  (interactive "P")
  (require 'erlang)
  (if (not (bufferp inferior-erlang-buffer-name))
      (run-erlang))
  (get-buffer-create inferior-erlang-buffer-name)
  )

(set-repl-handler! 'erlang-mode '+erlang/open-repl)


(defun +elixir/open-repl (&optional arg)
  (interactive "P")
  (require 'alchemist-iex)
  (if (not (bufferp elixir-shell-buffer-name))
      (alchemist-iex-run))
  (get-buffer-create elixir-shell-buffer-name)
  )

(set-repl-handler! 'elixir-mode '+elixir/open-repl)
