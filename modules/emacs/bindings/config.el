;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(load! "+repl-commands")

(after! evil
  (load! "+leader-bindings")
  (load! "+evil-bindings")
  (load! "+evil-submap-bindings")
  (load! "+ibuffer-bindings")
  (load! "+misc-bindings")
  )
(after! evil-ex
  (load! "+evil-ex-setup")
)

(use-package general-mod
  :after general)
(use-package which-mod
  :after which-key)

(after! flycheck
  (map! :leader
        :desc "Flycheck" "!" flycheck-command-map
        :prefix "c"
        :desc "Flycheck" "!" flycheck-command-map
        )
  )


(add-hook 'ibuffer-mode-hook #'+jg-ibuffer-update-hook)
