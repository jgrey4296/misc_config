;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map (conf-toml-mode-map toml-mode-map)
      :n "|" #'librarian-insert-trigger
      )
