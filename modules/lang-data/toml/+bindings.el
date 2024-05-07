;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map conf-toml-mode-map
      :n "|" #'general-insert-call
      )
