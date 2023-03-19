;;; domain-specific/bibtex/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+hooks")
(load! "tags/+tags")
(load! "+advice")
(load! "dired/+dired")
(load! "dialect/+entries")
(load! "dialect/+fields")
(after! jg-bindings-total
  (load! "evil/+motions")
  (load! "+bindings")
)
(after! hydra
  (load! "hydra/+hydra")
  )
(after! helm
  (load! "helm/+all")
  )

(use-package! bibtex
  :defer t
  :config
  (load! "util/+all")
  (load! "clean/+all")
  (defun +jg-bibtex-customisation-hook ()
    (add-hook 'jg-text-whitespace-clean-hook '+jg-bibtex-cleanup-ensure-newline-before-def 5 t)
    (add-hook 'jg-text-whitespace-clean-hook 'delete-trailing-whitespace 10 t)
    (add-hook 'jg-text-whitespace-clean-hook '+jg-text-cleanup-whitespace 20 t)
    )

  (add-hook! 'bibtex-mode-hook
             #'yas-minor-mode
             #'+jg-bibtex-font-lock-mod-hook
             #'+jg-bibtex-customisation-hook
             )

  (pushnew! bibtex-dialect-list 'jg)
  (let ((sorted-entries
         (sort (copy-alist bibtex-jg-entry-alist) (lambda (x y) (string-lessp (car x) (car y)))))
        )
    (setq bibtex-jg-entry-alist sorted-entries)
    )
  (add-hook 'bibtex-mode-hook #'(defun +jg-bibtex-set-dialect () (bibtex-set-dialect 'jg)))
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
  (setq org-ref-clean-bibtex-entry-hook jg-bibtex-clean-hooks)
  (remove-hook 'bibtex-mode-hook #'org-ref-bibtex-mode-keys)
  )

(use-package! oc
  :defer t
  :config
  (setq org-cite-global-bibliography
        (ensure-list
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography)))
        ;; Setup export processor; default csl/citeproc-el, with biblatex for
        ;; latex
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t)
  )
(use-package! oc-biblatex :after oc)
(use-package! oc-csl :after oc)
(use-package! oc-natbib :after oc)

(use-package! bibtex-completion
  :when (or (modulep! :completion ivy)
            (modulep! :completion helm))
  :defer t
  )
(use-package! ivy-bibtex
  :when (modulep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  )

(add-hook 'doom-first-file-hook #'bibtex-completion-init)
(add-hook 'doom-first-file-hook #'+jg-bibtex-build-list 90)
(add-hook 'doom-first-file-hook #'org-ref-version)
