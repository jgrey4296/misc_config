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

(use-package! general
  :config
  (defalias 'general-extended-def-:which-key #'+jg-binding-general-which-key-handler)
  )

(add-hook 'doom-first-input-hook #'+jg-binding-setup-leaders-hook -100)
(add-hook 'doom-first-input-hook #'+jg-binding-setup-leaderless-hook -100)
(add-hook 'doom-first-input-hook #'+jg-binding-evil-total-hook 100)

(add-hook! ibuffer-load
           #'+jg-binding-ibuffer-setup-hook
           #'+jg-binding-ibuffer-update-hook
           )

(add-hook 'evil-after-load-hook #'+jg-binding-evil-ex-setup-hook)
(add-hook 'evil-after-load-hook #'+jg-binding-evil-submap-hook)
