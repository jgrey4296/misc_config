
(load! "+vars")
(load! "util/+dired")
(load! "helm/+helm")
(load! "util/+index")
(load! "util/+tags")
(load! "util/+util")
(load! "+funcs")
(load! "+advice")
(after! ivy
  (load! "ivy/+ivy-actions")
  )
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! rawtag-mode)

(after! (evil org helm)
  (+jg-tag-rebuild-tag-database)

  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )
