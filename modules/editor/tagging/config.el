
(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(use-package! rawtag-mode
  :commands rawtag-mode
  )

(use-package! tagging-minor-mode
  :after (evil helm)
  :config
  (tagging-minor-mode-rebuild-tag-database)
  (global-tagging-minor-mode)
  )
