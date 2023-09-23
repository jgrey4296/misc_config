;; config.el<2> -*- mode: Elisp; lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(use-package! rawtag-mode
  :commands rawtag-mode
  )

(use-package! librarian-tagging-helm
  :after (evil helm)
  )
