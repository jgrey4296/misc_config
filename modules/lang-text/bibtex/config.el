;;; domain-specific/bibtex/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(advice-add 'org-ref-build-full-bibliography :override #'+jg-build-bibliography-a)
(advice-add 'bibtex-autokey-get-field        :around   #'+jg-bibtex-autokey-field-expand-a)
(advice-add 'bibtex-set-field                :override #'+jg-bibtex-set-field-a)
(advice-add 'org-ref-version                 :around   #'+jg-org-ref-version-override-a)
(advice-add 'bibtex-completion-init          :override #'+jg-bibtex-init-no-file-watchers-a)
(advice-add 'org-ref-clean-bibtex-entry      :around   #'+jg-bibtex-clean-dont-move-a)


(use-package! bibtex
  :commands bibtex-mode
  :config
  (local-load! "dialect/+entries")
  (local-load! "dialect/+fields")
  (pushnew! bibtex-dialect-list 'jg)

  (let ((sorted-entries (sort (copy-alist bibtex-jg-entry-alist) (lambda (x y) (string-lessp (car x) (car y)))))
        )
    (setq bibtex-jg-entry-alist sorted-entries)
    )


  (add-hook! 'bibtex-mode-hook
             #'display-line-numbers-mode
             #'yas-minor-mode
             #'outline-minor-mode
             #'+jg-bibtex-font-lock-mod-hook
             #'reftex-mode
             #'general-insert-minor-mode)

  (add-hook! 'bibtex-mode-hook :append
    (bibtex-set-dialect 'jg)
    (require 'helm)
    )

  )

(use-package! helm-bibtex
  :defer t
  :after bibtex
)

(use-package! ivy-bibtex
  :after bibtex
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  )

(use-package! bibtex-completion
  :after bibtex
  )

(use-package! org-ref
  :after bibtex
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  :config
  (setq-hook! 'bibtex-mode-hook
    org-ref-clean-bibtex-entry-hook jg-bibtex-clean-hooks
    )
  (remove-hook 'bibtex-mode-hook #'org-ref-bibtex-mode-keys)
  )

(use-package! oc
  :commands org-cite-insert
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

(use-package! bibtex-style
  :defer t
  :config
  (add-hook 'bibtex-style-mode-hook #'hs-minor-mode)
  (add-hook 'bibtex-style-mode-hook #'general-insert-minor-mode)
  )
