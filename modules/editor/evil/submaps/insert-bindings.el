;;; +evil-insert-state-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-insert-state-map
      "C-j"                       #'next-line
      "C-k"                       #'previous-line
      "C-u"                       #'universal-argument
      [escape] 'evil-normal-state
      :desc "Escape"        "C-g" #'evil-escape
      :desc "Delete"        "DEL" #'backward-delete-char
      "RET" #'newline
      "<f10>" #'evil-record-macro
      ;; TAB
      )

(map! :map jg-binding-replace-state-map
      "s" #'self-insert-command
      )
