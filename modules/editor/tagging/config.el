
(load! "+vars")
(load! "+funcs")
(load! "helm/+sources")
(load! "helm/+actions")
(load! "helm/+transformers")
(load! "helm/+helm")
(load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")
(after! jg-evil-ex-bindings
  (evil-define-operator +jg-tag-helm-start (beg end &rest rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end))
  (evil-ex-define-cmd "t[ag]"  #'tagging-minor-mode-tagger)
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
