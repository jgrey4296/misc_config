;;; config/default/config.el -*- lexical-binding: t; -*-

(local-load! "+spec-defs")
(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(add-hook 'doom-first-file-hook
          #'(lambda ()
              (advice-add 'display-warning :before-until #'+jg-default-display-warning-ad)
              ))

(use-package! spec-handling
  :commands (run-spec-handlers spec-handling-new! spec-handling-add! spec-handling-setq)
  )
