
(load! "+vars")
(load! "util/+dired")
(load! "util/+index")
(load! "util/+tags")
(load! "util/+util")
(load! "+funcs")
(load! "+advice")
(load! "helm/+sources")
(load! "helm/+actions")
(load! "helm/+transformers")
(load! "helm/+helm")

(after! ivy
  (load! "ivy/+ivy-actions")
  )
(after! jg-bindings-total
  (load! "+bindings")
  )
(after! (evil org)
  (+jg-tag-rebuild-tag-database)
  (require 'helm)
  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )

(use-package! rawtag-mode :defer t)
