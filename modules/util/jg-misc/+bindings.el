;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "b"
       :desc "Undo-Tree" "u" #'+jg-misc-undo-tree)
      (:prefix "t"
       "v r" #'rainbow-mode)
      )
(map! :map help-map
       "DEL" #'free-keys
      )

(map! :map free-keys-mode-map
      :desc "Change Buffer" :n "b" #'free-keys-change-buffer
      :desc "Revert Buffer" :n "g" #'revert-buffer
      :desc "Describe Mode" :n "h" #'describe-mode
      :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
      :desc "Quit"          :n "q" #'quit-window
      )
