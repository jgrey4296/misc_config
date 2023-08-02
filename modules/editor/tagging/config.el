
(load! "+vars")
(load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(add-hook! 'doom-first-file-hook #'global-tagging-minor-mode)

(use-package! rawtag-mode
  :commands rawtag-mode
  )

(use-package! tagging-minor-mode
  :after (evil helm)
  :config
  (tagging-minor-mode-rebuild-tag-database)
  )
