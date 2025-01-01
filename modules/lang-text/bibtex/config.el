;;; domain-specific/bibtex/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+extra")

(defer-load! jg-bindings-total "+bindings")
(local-load! "dialect/+entries")
(local-load! "dialect/+fields")

(advice-add 'org-ref-build-full-bibliography :override #'+jg-build-bibliography-a)
(advice-add 'bibtex-autokey-get-field        :around   #'+jg-bibtex-autokey-field-expand-a)
(advice-add 'bibtex-set-field                :override #'+jg-bibtex-set-field-a)
(advice-add 'org-ref-version                 :around   #'+jg-org-ref-version-override-a)
(advice-add 'bibtex-completion-init          :override #'+jg-bibtex-init-no-file-watchers-a)
(advice-add 'org-ref-clean-bibtex-entry      :around   #'+jg-bibtex-clean-dont-move-a)

(use-package! bibtex
  :commands bibtex-mode
  :config
  (pushnew! bibtex-dialect-list 'jg)
  (local-load! "+tags")

  (let ((sorted-entries (sort
                         (copy-alist bibtex-jg-entry-alist)
                         (lambda (x y) (string-lessp (car x) (car y)))))
        )
    (setq bibtex-jg-entry-alist sorted-entries)
    )

  (add-hook! 'bibtex-mode-hook
             #'display-line-numbers-mode
             #'yas-minor-mode
             #'outline-minor-mode
             #'librarian--biblio-font-lock-mod-hook
             #'reftex-mode
             #'librarian-insert-minor-mode)

  (add-hook! 'bibtex-mode-hook :append
    (bibtex-set-dialect 'jg)
    (require 'helm)
    )

  )

(use-package! helm-bibtex
  :when (modulep! :ui helm)
  :defer t
  :after bibtex
)

(use-package! ivy-bibtex
  :when (modulep! :ui ivy)
  :after bibtex
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  )

(use-package! org-ref
  :after bibtex
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  :config
  (remove-hook 'bibtex-mode-hook #'org-ref-bibtex-mode-keys)
  )

(use-package! bibtex-style
  :defer t
  :config
  (add-hook 'bibtex-style-mode-hook #'hs-minor-mode)
  (add-hook 'bibtex-style-mode-hook #'librarian-insert-minor-mode)
  )

(use-package! librarian--biblio-clean)

(use-package! librarian--biblio-edit)
