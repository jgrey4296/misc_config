;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-
(message "Loading modules/util/jg-misc/+bindings.el")

(defun +jg-misc-binding-hook ()
  (message "Setting up jg-misc bindings")
  (map! :leader
        (:prefix "b"
         :desc "Undo-Tree" "u" #'+jg-misc-undo-tree)
        (:prefix "w"
         :desc "Toggle Layout" "|" #'+jg-window-layout-toggle
         :desc "Rotate Windows" "\\" #'+jg-rotate-windows-forward
         )
        )

  (map! :map messages-buffer-mode-map
        :n "q" #'+popup/close
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
)
