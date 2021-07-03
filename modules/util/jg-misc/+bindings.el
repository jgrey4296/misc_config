;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up jg-misc bindings: %s" (current-time-string))
(map! :leader
      :desc "Have you Played?" "o h h" #'+jg-misc-helm-rps-have-you-playeds
      )

(evil-make-intercept-map messages-buffer-mode-map)

(map! :map help-map
      "DEL" #'free-keys
      )

(defun +jg-misc-free-key-binding-update ()
  (map! :map free-keys-mode-map
        :desc "Change Buffer" :n "b" #'free-keys-change-buffer
        :desc "Revert Buffer" :n "g" #'revert-buffer
        :desc "Describe Mode" :n "h" #'describe-mode
        :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
        :desc "Quit"          :n "q" #'quit-window
        )
  (evil-make-intercept-map free-keys-mode-map)
  )
