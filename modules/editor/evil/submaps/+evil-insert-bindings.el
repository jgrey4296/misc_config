;;; +evil-insert-state-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-insert-state-map
      "C-j"                       #'next-line
      "C-k"                       #'previous-line
      [escape] 'evil-normal-state
      :desc "Escape"        "C-g" #'evil-escape
      ;; TAB
      )
