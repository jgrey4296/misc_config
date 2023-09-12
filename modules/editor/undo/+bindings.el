;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-normal-state-map
      :after undo-fu
      :desc "undo" "u" 'undo-fu-only-undo
      :desc "redo" "U" 'undo-fu-only-redo
      )

(map! :map jg-binding-operator-map
      :desc "Vundo"                       "u" #'vundo
      )


(map! :map vundo-mode-map
      :n "RET" #'vundo-confirm
      )
