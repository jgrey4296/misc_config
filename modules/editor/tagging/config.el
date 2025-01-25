;; config.el<2> -*- mode: Elisp; lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(use-package! rawtag-mode
  :commands rawtag-mode
  )

(use-package! librarian-tag-helm
  :commands librarian-tag-helm
  )

(use-package! librarian-tag-ivy
  :defer t
  )

(use-package! subfile-mode
  :commands subfile-mode
  )
