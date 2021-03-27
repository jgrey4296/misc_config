;;; util/text/config.el -*- lexical-binding: t; -*-
(use-package! academic-phrases
  :defer t
  )
(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(load! "+vars")
(load! "+funcs")
(load! "+barchart")

(after! evil
  (load! "+bindings")
  )
