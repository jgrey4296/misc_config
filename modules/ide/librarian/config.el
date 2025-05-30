;;; tools/lookup/config.el -*- lexical-binding: t; -*-

(local-load! "+spec-defs")
(local-load! "+vars")
(local-load! "+extra")
(local-load! "+spelling")
(local-load! "+envs")
(local-load! "+tags")

(defer-load! jg-bindings-core "+bindings")

(use-package! librarian
  :commands (librarian-mode librarian-url global-librarian-mode librarian-insert-minor-mode)
  :after transient-toggles
  :hook (doom-first-input . global-librarian-mode)
  :config
  (add-hook 'jg-ui-transient-toggles-hook #'+jg-librarian-build-librarian-transient 90)
  (add-hook 'speckler-hook #'librarian-tag-mode-rebuild-tag-database)
  (setq librarian--browse-default "firefox")
  ;; choose backends?
  )

(use-package! xref
  :config
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (advice-add 'projectile-find-tag :around #'+lookup--projectile-find-tag-a)

  )
