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
  (defadvice! +lookup--projectile-find-tag-a (fn)
    :around #'projectile-find-tag
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall fn)))

  )

(use-package! ivy-xref
  :config
  ;; HACK Fix #4386: `ivy-xref-show-xrefs' calls `fetcher' twice, which has
  ;; side effects that breaks in some cases (i.e. on `dired-do-find-regexp').
  (defadvice! +lookup--fix-ivy-xrefs (fn fetcher alist)
    :around #'ivy-xref-show-xrefs
    (when (functionp fetcher)
      (setf (alist-get 'fetched-xrefs alist)
            (funcall fetcher)))
    (funcall fn fetcher alist))

  )

(use-package! dash-docs
  :defer t
  :config
  (setq dash-docs-enable-debugging init-file-debug
        dash-docs-docsets-path (expand-file-name "~/.cache/docsets")
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
  :commands (librarian-mode librarian-url)
  :after transient-toggles
  :hook (doom-first-input . global-librarian-mode)
  :init
  (transient-make-mode-toggle! librarian-mode "Librarian" "b")
  (transient-append-suffix 'jg-toggle-main "c" '("b" transient-macro-toggle-librarian-mode))
  :config
  (+jg-librarian-add-librarian-transient)
  (librarian-tagging-mode-rebuild-tag-database)
  (global-librarian-tagging-mode)
  ;; choose backends?
  )

(use-package! company-gtags)

(use-package! company-ispell)

(use-package! company-keywords)
