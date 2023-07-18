;;; domain-specific/bibtex/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")
;; (defer! 240 ;; when idle for 4 minutes
;;   (unless jg-bibtex-helm-candidates
;;     (require 'helm)
;;     (require 'helm-source)
;;     (require 'helm-bibtex)
;;     (+jg-bibtex-build-list)
;;     (bibtex-completion-clear-cache)
;;     (bibtex-completion-init)
;;     ;; (mapcar #'+jg-bibtex-process-candidates (bibtex-completion-candidates))
;;     )
;;   )

(use-package! bibtex
  :commands bibtex-mode
  :config
  (load! "dialect/+entries")
  (load! "dialect/+fields")
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
  (setq-hook! 'bibtex-mode-hook org-ref-clean-bibtex-entry-hook jg-bibtex-clean-hooks)
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
