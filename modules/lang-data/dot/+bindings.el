;;; +bindings.el -*- lexical-binding: t; -*-

(map! :after graphviz-dot-mode
      :map graphviz-dot-mode-map
      :n "RET" #'+jg-dot-compile-and-view
      :localleader
      :n "e" #'+jg-dot-set-ext
      )
