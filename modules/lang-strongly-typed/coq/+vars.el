;;; +vars.el -*- lexical-binding: t; -*-


(after! proof-general
    (setq proof-splash-enable nil
          proof-three-window-enable nil
          coq-compile-before-require t
          coq-accept-proof-using-suggestion 'never
          )
)


(spec-handling-add! popup
                    '(coq
                     ("^\\*\\(?:response\\|goals\\)\\*" :ignore t)
                     )
                    )
(spec-handling-add! lookup-handler
                    `(company-coq-mode
                      :definition ,#'company-coq-jump-to-definition
                      :references ,#'company-coq-grep-symbol
                      :documentation ,#'company-coq-doc
                      )
                    )
(spec-handling-add! auto-modes
                    '(coq
                      ("\\.v\\'" . coq-mode)
                      )
                    )
