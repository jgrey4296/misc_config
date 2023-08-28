;;; lang/idris/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")


(use-package! idris-mode
  :commands idris-mode
  :config
  (add-hook 'idris-mode-hook #'turn-on-idris-simple-indent)
  )
