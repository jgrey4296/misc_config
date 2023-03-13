;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(load! "+repl-commands")
(load! "util/+cleaning")

(after! (evil general-mod which-mod)
  (load! "+leader-bindings")
  (load! "evil/+evil-bindings")
  (load! "evil/+evil-submap-bindings")
  (load! "+misc-bindings")
  (provide 'jg-bindings-total)
  )

(after! evil-ex
  (load! "evil/+evil-ex-setup")
)

(use-package! general-mod
  :after general)

(use-package! which-mod
  :after which-key)

(after! flycheck
  (map! :leader
        :desc "Flycheck" "!" flycheck-command-map
        :prefix "c"
        :desc "Flycheck" "!" flycheck-command-map
        )
  )
