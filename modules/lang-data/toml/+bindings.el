;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map conf-toml-mode-map
      :desc "open dooter" :n "s >" #'+jg-toml-open-dooter
      :n "|" #'general-insert-call
      )
