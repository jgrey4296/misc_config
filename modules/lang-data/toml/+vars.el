;;; +vars.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "dooter.py" "doot.toml")
  )

;; (spec-handling-add! projects
;;                     `(jg-toml-project ("doot.toml") :project-file "doot.toml" :related-files-fn ,#'+jg-toml-related-files-fn)
;;                     )

(spec-handling-add! whitespace-cleanup
                    `(conf-toml-mode
                      ,#'+jg-toml-cleanup-ensure-newline-before-table
                      ,#'delete-trailing-whitespace
                      ,#'+jg-text-cleanup-whitespace
                      )
                    )

(spec-handling-add! lookup-regular
                    '(conf-toml-mode
                      ("toml spec" . "https://toml.io/en/v1.0.0")
                      )
                    )

(spec-handling-add! auto-modes
                    '(toml
                      ("\\.toml\\'" . conf-toml-mode)
                      )
                    )
