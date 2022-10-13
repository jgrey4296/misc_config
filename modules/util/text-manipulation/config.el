;;; util/text/config.el -*- lexical-binding: t; -*-
(load! "+vars")
(load! "+funcs")
(load! "+barchart")
(after! evil
  (load! "+operators")
  (load! "+motions")
  (load! "+state")
  (load! "+bindings")
  (load! "+advice")
)

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
(use-package! highlight-parentheses
  :defer t
  )
(use-package! rainbow-mode
  :defer t
  :init
  (add-hook! 'prog-mode-hook 'rainbow-mode)
)
(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(use-package! helm-wordnet :defer t)
(use-package! wordnut
  :defer t
  :init
  (add-hook 'wordnut-mode-hook 'outline-minor-mode)

  )
