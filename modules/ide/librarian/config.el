;;; tools/lookup/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-core "+bindings")

(use-package! xref
  :config
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (advice-add 'projectile-find-tag :around #'+lookup--projectile-find-tag-a)

  )

(use-package! ivy-xref
  :config
  (advice-add 'ivy-xref-show-xrefs :around #'+lookup--fix-ivy-xrefs)
  )

(use-package! dash-docs
  :defer t
  :config
  (setq dash-docs-enable-debugging init-file-debug
        dash-docs-docsets-path (expand-file-name "~/_cache_/docsets")
        dash-docs-min-length 2
        dash-docs-browser-func #'browse-url
        )

  (require 'counsel-dash nil t)
)

;;-- words

(use-package! wordnut
  :defer t
  :init
  (add-hook 'wordnut-mode-hook 'outline-minor-mode)
  )

(use-package! osx-dictionary
              :when (eq system-type 'darwin)
              :defer t)

;; (use-package! powerthesaurus :defer t)

(use-package! synosaurus :defer t)

(use-package! helm-wordnet :defer t)

(use-package! define-word :defer t)

;;-- end words

(use-package! librarian
  :commands (librarian-mode librarian-url global-librarian-mode)
  :after transient-toggles
  :hook (doom-first-input . global-librarian-mode)
  :config
  (+jg-librarian-add-librarian-transient)
  (librarian-tagging-mode-rebuild-tag-database)
  (global-librarian-tagging-mode)
  (setq librarian-default-browser "firefox")
  ;; choose backends?
  )

(use-package! company-gtags)

(use-package! company-ispell)

(use-package! company-keywords)
