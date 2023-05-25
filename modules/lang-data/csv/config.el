;;; lang/data/config.el -*- lexical-binding: t; -*-

(after! (jg-bindings-total csv-mode)
  (load! "+bindings")
  )

(use-package! csv-mode
  :defer t)
