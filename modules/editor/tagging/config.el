
(load! "+vars")
(load! "+funcs")
(load! "helm/+sources")
(load! "helm/+actions")
(load! "helm/+transformers")
(load! "helm/+helm")

(after! jg-bindings-total
  (load! "+bindings")
  )
(after! evil-ex
  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )

(use-package! rawtag-mode
  :commands rawtag-mode
  )
(use-package! tagging-minor-mode
  :after (evil helm)
  :config
  (tagging-minor-mode-rebuild-tag-database)
  )

(add-hook! 'doom-first-file-hook #'global-tagging-minor-mode)

(spec-handling-new-hooks! tagging
                          (setq-local tagging-minor-mode-handlers
                                      (list :new (plist-get val :new)
                                            :set (plist-get val :set)
                                            :get (plist-get val :get)
                                            :buff (plist-get val :buff)
                                            )
                                      )
                          )
