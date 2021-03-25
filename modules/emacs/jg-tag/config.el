(after! tag-clean-minor-mode ()
  (push 'tag-clean-minor-mode minor-mode-list)
  )
(after! evil
  (load! "+bindings")
  )
(after! ivy (load! "+ivy_actions"))

(load! "+dired")
(load! "+helm")
(load! "+index")
(load! "+tags")
(load! "+util")
(load! "+vars")

(use-package! bibtex
  :init
  (add-hook 'bibtex-mode-hook #'org-ref-version)
  )
(use-package! tag-clean-minor-mode
  :defer t)
(use-package! tag-mode
  :defer t)
(use-package! helm-bibtex
  :defer t
  :commands (bibtex-completion-init)
)
(use-package! org-ref
  :after-call org-ref-version
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  (add-hook 'bibtex-mode-hook #'yas-minor-mode)
  (add-hook 'bibtex-mode-hook #'reftex-mode)
  :config
  (loop for a-hook in jg-bibtex-clean-remove-hooks
        do (remove-hook 'org-ref-clean-bibtex-entry-hook a-hook))
  (loop for a-hook in jg-bibtex-clean-add-hooks
        do (add-hook 'org-ref-clean-bibtex-entry-hook a-hook 100))
  )
(after! (evil org helm)
  (jg-tag-rebuild-tag-database)

  (evil-define-operator jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (jg-tag-helm-tagger beg end)
    )
  )
(after! (f helm-bibtex)
  (jg-tag-build-bibtex-list)
)
