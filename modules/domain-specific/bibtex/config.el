;;; domain-specific/bibtex/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+helm")
(load! "+hooks")
(load! "+vars")
(load! "+helm")
(load! "+tags")
(load! "+hydra")
(after! evil
  (load! "+bindings")
  )


(use-package! bibtex
  :init
  (add-hook! 'bibtex-mode-hook #'+jg-bibtex-tag-setup-hook #'yas-minor-mode #'org-ref-version)
  )
(use-package! helm-bibtex
  :defer t
  :commands (bibtex-completion-init)
)
(use-package! org-ref
  :after-call org-ref-version
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  (add-hook 'bibtex-mode-hook #'reftex-mode)
  :config
  (remove-hook! 'org-ref-clean-bibtex-entry-hook jg-bibtex-clean-remove-hooks)
  (add-hook!    'org-ref-clean-bibtex-entry-hook :append jg-bibtex-clean-add-hooks)
  )

(after! (f helm-bibtex)
  (+jg-bibtex-build-list)
)

