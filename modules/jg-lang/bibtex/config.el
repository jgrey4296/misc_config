;;; domain-specific/bibtex/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+clean-funcs")
(load! "+helm")
(load! "+hooks")
(load! "+vars")
(load! "+helm")
(load! "+tags")
(load! "+hydra")
(load! "+advice")
(load! "+dired")
(load! "+entries")
(load! "+fields")
(after! jg-bindings-total
  (load! "+motions")
  (load! "+bindings")
)

(use-package! bibtex
  :init
  (add-hook! 'bibtex-mode-hook
             #'yas-minor-mode
             #'+jg-bibtex-font-lock-mod-hook
             )
  )
(use-package! helm-bibtex
  :commands (bibtex-completion-init)
)

(use-package-hook! bibtex-completion :post-config
  (provide 'jg-bibtex-vars-go)
  )

(use-package! org-ref
  :after-call org-ref-version
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  (add-hook 'bibtex-mode-hook #'reftex-mode)
  :config
  (setq org-ref-clean-bibtex-entry-hook jg-bibtex-clean-hooks)

  )

(add-hook 'doom-first-file-hook #'bibtex-completion-init)
(add-hook 'doom-first-file-hook #'+jg-bibtex-build-list 90)
(add-hook 'doom-first-file-hook #'org-ref-version)

(after! bibtex
  (pushnew! bibtex-dialect-list 'jg)
  (add-hook 'bibtex-mode-hook #'(lambda () (bibtex-set-dialect 'jg)))
  (remove-hook 'bibtex-mode-hook #'org-ref-bibtex-mode-keys)
  )
