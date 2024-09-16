;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! popup
                    '(lsp
                      ("^\*lsp session\*"  :side right  :ttl nil :width 0.5 :quit t :select nil :priority 50)
                      ("^\\*lsp-\\(help\\|install\\)" :size 0.35 :quit t :select t)
                      ("^\\*eglot-help" :size 0.15 :quit t :select t)
                     )
                    '(flycheck
                      ("^\\*Flycheck error messages\\*" :select nil)
                      ("^\\*Flycheck errors\\*" :size 0.25)
                      )
                    )

(spec-handling-add! fold
                    `(lsp-browser
                     :modes (lsp-browser-mode)
                     :priority 30
                     :triggers (:open-all   ,#'+jg-lsp-toggle-widget-on-line
                                :close-all  ,#'+jg-lsp-toggle-widget-on-line
                                :toggle     ,#'+jg-lsp-toggle-widget-on-line
                                :open       ,#'+jg-lsp-toggle-widget-on-line
                                :open-rec   ,#'+jg-lsp-toggle-widget-on-line
                                :close      ,#'+jg-lsp-toggle-widget-on-line
                                )
                     )
                    )

(spec-handling-add! lookup-handler
                    `(lsp-mode
                     :definition          +lsp-lookup-definition-handler
                     :declaration         lsp-find-declaration
                     :references          +lsp-lookup-references-handler
                     :documentation       lsp-describe-thing-at-point
                     :implementations     lsp-find-implementation
                     :type-definition     lsp-find-type-definition
                     )
                    `(eglot--managed-mode
                     :definition          xref-find-definitions
                     :references          xref-find-references
                     :implementations     eglot-find-implementation
                     :type-definition     eglot-find-typeDefinition
                     :documentation       +eglot-lookup-documentation
                     )
                    `(lsp-ui-mode
                      :definition         lsp-ui-peek-find-definitions
                      :implementations    lsp-ui-peek-find-implementation
                      :references         lsp-ui-peek-find-references
                      :async t
                      )
                    )

(spec-handling-add! tree-sit-lang
                    '(agda-mode       . agda)
                    '(c-mode          . c)
                    '(c++-mode        . cpp)

                    '(elm-mode        . elm)

                    '(julia-mode      . julia)
                    '(ruby-mode       . ruby)
                    '(tuareg-mode     . ocaml)
                    )

(spec-handling-add! env-handling
                    '(flycheck
                      (:support flycheck #'(lambda (path name)
                                             (when (featurep 'flycheck)
                                               (unless flycheck-enabled-checkers
                                                 (let ((chosen (intern (ivy-read "Flychecker: " flycheck-disabled-checkers :require-match t))))
                                                   (delete chosen flycheck-disabled-checkers)
                                                   (add-to-list flycheck-enabled-checkers chosen)
                                                   ))
                                               (add-hook 'python-mode-hook #'flycheck-mode)
                                               )
                                             )
                                (-partial #'flycheck-mode -1)
                                )
                      (:teardown flycheck (-partial flycheck-mode -1))
                      )
                      )

(spec-handling-add! env-handling
                    '(lsp
                      (:support lsp
                                #'(lambda (state) (when (featurep 'lsp) (add-hook 'python-mode-hook #'lsp-deferred)))
                                #'(lambda (state)
                                    (when (featurep 'lsp)
                                      (when lsp-mode (lsp-mode -1))
                                      (when lsp--last-active-workspaces
                                        (lsp-workspace-shutdown (car lsp--last-active-workspaces)))
                                      (remove-hook 'python-mode-hook #'lsp-deferred)
                                      )
                                     )
                                )
                      (:teardown lsp #'(lambda (state) (when (featurep 'lsp) (lsp-disconnect))))
                      )
                    )

(spec-handling-add! env-handling
                    '(eglot
                      (:support eglot
                                #'(lambda (state) (when (featurep 'eglot) (add-hook 'python-mode-hook #'eglot-ensure)))
                                #'(lambda (state)
                                    (when (featurep 'eglot) (remove-hook 'python-mode-hook #'eglot-ensure)))
                                )
                      )
                    )

(spec-handling-add! env-handling
                    '(semantic
                      (:support semantic
                                #'(lambda (state) (when (featurep 'semantic) (add-hook 'python-mode-hook #'semantic-mode)))
                                #'(lambda (state) (when (featurep 'semantic) (remove-hook 'python-mode-hook #'semantic-mode)))
                                )
                      )
                    )
