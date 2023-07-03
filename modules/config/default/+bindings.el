;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map jg-help-map
      :after jg-help-bindings
      "r s" #'run-spec-handlers
      "d S" #'spec-handling-report
      "d s" #'spec-handling-describe
      )


(map! :leader
      :prefix "b"
      :n "?" #'+jg-default-debug-auto-mode
      )
