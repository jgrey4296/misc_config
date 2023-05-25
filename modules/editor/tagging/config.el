
(load! "+vars")
(load! "+funcs")
(load! "helm/+sources")
(load! "helm/+actions")
(load! "helm/+transformers")
(load! "helm/+helm")
(load! "+spec-defs")

(after! jg-bindings-total
  (load! "+bindings")
  )
(after! evil-ex
  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )
(add-hook! 'doom-first-file-hook #'global-tagging-minor-mode)

(use-package! rawtag-mode
  :commands rawtag-mode
  )

(use-package! tagging-minor-mode
  :after (evil helm)
  :config
  (tagging-minor-mode-rebuild-tag-database)
  )
