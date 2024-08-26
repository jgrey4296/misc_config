;; config.el<2> -*- mode: Elisp; lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(use-package! rawtag-mode
  :commands rawtag-mode
  )

(use-package! librarian-tagging-helm
  :commands librarian-tagging-helm
  )

(use-package! librarian-tagging-ivy
  :defer t
  )
