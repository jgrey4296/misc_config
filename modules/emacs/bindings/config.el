;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(load! "+leader-bindings")
(load! "+leaderless-bindings")
(load! "+evil-ex-setup")
(load! "+evil-bindings")
(load! "+evil-bindings-2")
(load! "+ibuffer-bindings")
(load! "+which-key-update")

(after! flycheck
  (map! :leader
        :desc "Flycheck" "!" flycheck-command-map
        :prefix "c"
        :desc "Flycheck" "!" flycheck-command-map
        )
  )

(add-hook! doom-first-input :append
           #'+jg-binding-setup-leaders-hook
           #'+jg-binding-setup-leaderless-hook
           #'+jg-binding-setup-evil-hook
  ;;         #'+jg-binding-evil-finalise-hook
)

(add-hook! ibuffer-load
           #'+jg-binding-ibuffer-setup-hook
           ;;#'+jg-binding-ibuffer-update-hook
           )

(add-hook! evil-after-load
           #'+jg-binding-backup-evil-maps-hook
           #'+jg-binding-evil-ex-setup-hook
           )
