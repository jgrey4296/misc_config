;;; +vars.el -*- lexical-binding: t; -*-



(spec-handling-add! company
                    '(tuareg-mode #'merlin-company-backend)
                    '(sml-mode #'company-mlton-grouped-backend)
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
