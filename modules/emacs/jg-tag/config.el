(after! tag-clean-minor-mode ()
  (push 'tag-clean-minor-mode minor-mode-list)
  )
(after! ivy 
  (load! "+ivy_actions"))

(load! "+bindings")
(load! "+dired")
(load! "+helm")
(load! "+index")
(load! "+tags")
(load! "+util")
(load! "+vars")

(use-package! tag-clean-minor-mode
  :defer t)
(use-package! tag-mode
  :defer t)
(after! (evil org helm)
  (+jg-tag-rebuild-tag-database)

  (evil-define-operator +jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (+jg-tag-helm-tagger beg end)
    )
  )

(add-hook! doom-first-input
           #'+jg-tag-binding-hook
           #'+jg-tag-dired-binding-hook)

(add-hook! evil-after-load
           #'+jg-tag-evil-binding-hook
           )
