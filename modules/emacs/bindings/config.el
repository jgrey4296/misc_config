;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+misc")
(load! "+funcs")
(load! "+vars")
(after! evil
  (message "Post Evil")
  (load! "+leader-bindings")
  (load! "+leaderless-bindings")
  (load! "+evil-bindings")
  )
(after! (evil ibuffer)
  (message "Post Evil and Ibuffer")
  (load! "+ibuffer-bindings")
  )
(after! (evil which-key)
  (message "Post evil and which key")
  (load! "+which-key-update")
  ;;(+jg-binding-finalise)
  )

(after! flycheck
  (map! :leader
        :desc "Flycheck" "!" flycheck-command-map
        :prefix "c"
        :desc "Flycheck" "!" flycheck-command-map
        )
  )
