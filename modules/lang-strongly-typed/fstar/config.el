;;; lang/fstar/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! fstar-mode
  :commands fstar-mode
  )
