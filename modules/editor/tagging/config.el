
(load! "+vars")
(load! "util/+dired")
(load! "util/+index")
(load! "util/+tags")
(load! "util/+util")
(load! "+funcs")
(load! "+advice")
(after! helm
  (load! "helm/+helm")
  )
(after! ivy
  (load! "ivy/+ivy-actions")
  )
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! rawtag-mode :defer t)

(after! (evil org helm)
  (+jg-tag-rebuild-tag-database)

  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )
