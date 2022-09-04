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
(after! evil
  (load! "+motions")
  (load! "+bindings")
)

(use-package! bibtex
  :init
  (add-hook! 'bibtex-mode-hook
             #'+jg-bibtex-tag-setup-hook
             #'yas-minor-mode
             #'org-ref-version
             #'+jg-bibtex-font-lock-mod-hook
             #'+jg-ui-toggle-line-numbers)
  )
(use-package! helm-bibtex
  :commands (bibtex-completion-init)
)
(use-package! org-ref
  :after-call org-ref-version
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  (add-hook 'bibtex-mode-hook #'reftex-mode)
  :config
  (setq org-ref-clean-bibtex-entry-hook nil)
  (setq org-ref-clean-bibtex-entry-hook jg-bibtex-clean-hooks)

  ;; (loop for hook in jg-bibtex-clean-hooks
  ;;       do (add-hook 'org-ref-clean-bibtex-entry-hook hook 100))
  )

(add-hook 'doom-first-input-hook #'+jg-bibtex-build-list)
