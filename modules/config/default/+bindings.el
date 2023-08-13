;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map jg-help-map
      :after jg-help-bindings
      :prefix ("s" . "Spec Handlers")
      "r" #'spec-handling-report
      "d" #'spec-handling-describe
      )


(map! :leader
      :prefix "b"
      :n "?" #'+jg-default-debug-auto-mode
      )
