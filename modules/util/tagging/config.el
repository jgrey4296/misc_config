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
(after! evil
  (load! "+bindings")
  )

(use-package! tag-clean-minor-mode :defer t)
(use-package! tag-mode :defer t)
(use-package! tag-timeline-mode :defer t)

(after! (evil org helm)
  (+jg-tag-rebuild-tag-database)

  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )
(after! tag-clean-minor-mode
  (push 'tag-clean-minor-mode minor-mode-list)
  )
