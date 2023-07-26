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

(spec-handling-add! lookup-regular
                    '(ocaml-mode
                      ("OCaml Reference" . "https://v2.ocaml.org/releases/5.0/htmlman/index.html")
                      ("Ocaml Tutorial" . "https://ocaml.org/docs/up-and-running")
                      )
                    '(sml-mode
                      ("SML Docs" . "https://smlfamily.github.io/")
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

(spec-handling-add! eval
                    `(sml-mode :start ,#'run-sml)
                    `(tuareg-mode
                      :start ,#'utop
                      :send ,#'utop-eval-region
                      )
                    )
