;;; +vars.el -*- lexical-binding: t; -*-

(after! smartparens-ml
  (sp-with-modes '(tuareg-mode fsharp-mode)
    (sp-local-pair "(*" "*)" :actions nil)
    (sp-local-pair "(*" "*"
                   :actions '(insert)
                   :post-handlers '(("| " "SPC") ("|[i]*)[d-2]" "RET")))))

(spec-handling-add! tree-sit-lang
                    '(ocaml-mode      . ocaml)
                    )
(spec-handling-add! company
                    '(tuareg-mode merlin-company-backend)
                    '(sml-mode company-mlton-grouped-backend)
                    )
(spec-handling-add! lookup-handler
                    '(tuareg-mode :async t
                      :definition merlin-locate
                      :references merlin-occurrences
                      :documentation merlin-document
                      )
                    )

(spec-handling-add! popup
                    '(ml-langs
                      ("^\\*utop\\*" :quit nil)
                      )
                    )
(spec-handling-add! auto-modes
                    '(ml-langs
                      ( "\\.s\\(?:ml\\|ig\\)\\'" . sml-mode)
                      )
                    )

(spec-handling-add! repl
                    '(sml-mode :start run-sml)
                    '(tuareg-mode
                      :start utop
                      :send utop-eval-region
                      )
                    )
