;;; util/text/config.el -*- lexical-binding: t; -*-
(use-package! academic-phrases
  :defer t
  )
(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )
(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs lorem-ipsum-insert-list)
)
(load! "+vars")
(load! "+funcs")
(load! "+barchart")

(after! evil
  (load! "+bindings")
  (load! "+motions")
  )
