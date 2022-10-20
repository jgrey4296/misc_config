(load! "+dired")
(load! "+helm")
(load! "+index")
(load! "+tags")
(load! "+util")
(load! "+vars")
(load! "+funcs")
(load! "+advice")
(after! ivy
  (load! "+ivy-actions")
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
