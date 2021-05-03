;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(load! "+which-key-update")

(after! evil
  (load! "+leader-bindings")
  (load! "+leaderless-bindings")
  (load! "+evil-bindings")
  (load! "+evil-bindings-2")
  (load! "+ibuffer-bindings")
)
(after! evil-ex
  (load! "+evil-ex-setup")
  (+jg-binding-evil-ex-setup-hook)
  )


(after! flycheck
  (map! :leader
        :desc "Flycheck" "!" flycheck-command-map
        :prefix "c"
        :desc "Flycheck" "!" flycheck-command-map
        )
  )

(add-hook 'doom-first-input-hook #'+jg-binding-setup-leaders-hook -100)
(add-hook 'doom-first-input-hook #'+jg-binding-setup-leaderless-hook -100)
(add-hook 'doom-first-input-hook #'+jg-binding-evil-total-hook 100)
(add-hook 'doom-first-input-hook #'+jg-binding-evil-submap-hook)

(add-hook! ibuffer-mode #'+jg-ibuffer-setup-hook)
(add-hook! ibuffer-mode #'+jg-ibuffer-update-hook)
