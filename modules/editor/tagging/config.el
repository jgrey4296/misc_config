
(load! "+vars")
(load! "util/+index")
(load! "+funcs")
(load! "helm/+sources")
(load! "helm/+actions")
(load! "helm/+transformers")
(load! "helm/+helm")

(after! jg-bindings-total
  (load! "+bindings")
  )
(after! (evil org)
  ;; (+jg-tag-rebuild-tag-database)
  (require 'helm)
  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )

(use-package! rawtag-mode :defer t)
