;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix "t"
       :desc "Semantic" "S" #'semantic-mode
      )

(map! :map semantic-mode-map
      :after semantic
      :localleader
      :prefix ("^" . "Semantic")
      (:prefix ("t" . "toggle")
       :desc "Stick-func"     "s" #'semantic-stickyfunc-mode
       :desc "Highlight-func" "h" #'semantic-highlight-func-mode
       )

      )
