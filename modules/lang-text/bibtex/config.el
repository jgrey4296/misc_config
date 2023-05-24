;;; domain-specific/bibtex/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "dialect/+entries")
(load! "dialect/+fields")
(after! jg-bindings-total
  (load! "evil/+motions")
  (load! "+bindings")
)

(use-package! bibtex
  :defer t
  :config
  (pushnew! bibtex-dialect-list 'jg)
  (let ((sorted-entries (sort (copy-alist bibtex-jg-entry-alist) (lambda (x y) (string-lessp (car x) (car y)))))
        )
    (setq bibtex-jg-entry-alist sorted-entries)
    )

  (add-hook! 'bibtex-mode-hook
             #'yas-minor-mode
             #'outline-minor-mode
             #'+jg-bibtex-font-lock-mod-hook
             #'reftex-mode)

  (add-hook! 'bibtex-mode-hook :append
    (bibtex-set-dialect 'jg)
    (require 'helm)
    )

  )

(use-package! helm-bibtex
  :commands (bibtex-completion-init)
)

(use-package! ivy-bibtex
  :when (modulep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  )

(use-package! bibtex-completion
  :when (or (modulep! :completion ivy)
            (modulep! :completion helm))
  :defer t
  )

(use-package! org-ref
  :after bibtex
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  :config
  (setq-hook! 'bibtex-mode-hook org-ref-clean-bibtex-entry-hook jg-bibtex-clean-hooks)
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

(add-hook 'doom-first-file-hook #'bibtex-completion-init)
(add-hook 'doom-first-file-hook #'+jg-bibtex-build-list 90)
(autoload '+jg-bibtex-hydra/body (file-name-concat (dir!) "autoload" "hydra.el") t)
