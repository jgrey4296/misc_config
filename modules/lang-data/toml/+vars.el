;;; +vars.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "doot.toml")
  )

;; (speckler-add! projects
;;                     `(jg-toml-project ("doot.toml") :project-file "doot.toml" :related-files-fn ,#'+jg-toml-related-files-fn)
;;                     )

(speckler-add! whitespace-cleanup
                    `(conf-toml-mode
                      ,#'+jg-toml-cleanup-ensure-newline-before-table
                      ,#'delete-trailing-whitespace
                      ,#'+jg-text-cleanup-whitespace
                      )
                    )
(speckler-add! auto-modes
                    '(toml
                      ("\\.toml\\'" . conf-toml-mode)
                      )
                    '(conf
                      ("\\.conf\\'" . conf-mode)
                      ("\\.cnf\\'" . conf-mode)
                      ("\\.cfg\\'" . conf-mode)
                      )
                    )
(speckler-add! fold
                    `(toml
                      :modes (conf-toml-mode toml-mode toml-ts-mode)
                      :priority -50
                      :triggers (:open-all   ,#'outline-show-all
                                 :close-all  ,(cmd! (with-no-warnings (outline-hide-sublevels 1)))
                                 :toggle     ,#'outline-toggle-children
                                 :open       ,(cmd! (with-no-warnings (outline-show-entry) (outline-show-children)))
                                 :open-rec   ,#'outline-show-subtree
                                 :close      ,#'outline-hide-subtree
                                 )
                      )
                    )
(speckler-add! org-src
                    '(toml
                      ("toml" . toml)
                      )
                    )
