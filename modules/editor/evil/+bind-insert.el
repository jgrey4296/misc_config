;;; +evil-insert-state-bindings.el -*- lexical-binding: t; -*-

(set-keymap-parent jge-insert-state-map evil-insert-state-map)
(set-keymap-parent jge-replace-state-map jge-insert-state-map)

(map! :map jge-insert-state-map
      "C-j"                       #'next-line
      "C-k"                       #'previous-line
      "C-u"                       #'universal-argument
      [escape]                    'evil-normal-state
      :desc "Escape"        "C-g" #'evil-escape
      :desc "Delete"        "DEL" #'backward-delete-char
      "RET" #'newline
      "<f10>" #'evil-record-macro
      ;; TAB
      )

(map! :map jge-replace-state-map
      "s" #'self-insert-command
      )
