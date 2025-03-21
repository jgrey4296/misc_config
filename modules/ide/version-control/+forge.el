;;; +forge.el -*- lexical-binding: t; -*-

(use-package! forge
  :disabled t
  :after magit
  :config
  )

(use-package! forge
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :commands (forge-create-pullreq forge-create-issue)
  :preface
  (setq forge-database-file (concat doom-data-dir "forge/forge-database.sqlite"))
  (setq forge-add-default-bindings nil)
  :config
  (advice-add 'forge-dispatch                           :before #'+magit--forge-build-binary-lazily-a)
  (advice-add 'forge-get-repository                     :before-while #'+magit--forge-get-repository-lazily-a)
  )

(use-package! code-review
  :after magit
  :init
  ;; TODO This needs to either a) be cleaned up or better b) better map things
  ;; to fit
  (after! evil-collection-magit
    (dolist (binding evil-collection-magit-mode-map-bindings)
      (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
        (dolist (state states)
          (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
    )
  (speckler-add! evil-initial ()
    '(code-review-mode evil-default-state)
    )
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-code-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-code-review))))
