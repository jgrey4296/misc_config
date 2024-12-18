;;; +vars.el -*- lexical-binding: t; -*-

(after! smartparens-ml
  (sp-with-modes '(tuareg-mode fsharp-mode)
    (sp-local-pair "(*" "*)" :actions nil)
    (sp-local-pair "(*" "*"
                   :actions '(insert)
                   :post-handlers '(("| " "SPC") ("|[i]*)[d-2]" "RET")))))

(speckler-add! tree-sit-lang ()
  '(ocaml-mode      . ocaml)
  )
(speckler-add! company ()
  '(tuareg-mode merlin-company-backend)
  '(sml-mode company-mlton-grouped-backend)
  )
(speckler-add! lookup-handler ()
  '(tuareg-mode :async t
    :definition merlin-locate
    :references merlin-occurrences
    :documentation merlin-document
    )
  )

(speckler-add! popup ()
  '(ml-langs
    ("^\\*utop\\*" :quit nil)
    )
  )
(speckler-add! auto-modes ()
  '(ml-langs
    ( "\\.s\\(?:ml\\|ig\\)\\'" . sml-mode)
    )
  )

(speckler-add! repl ()
  '(sml-mode :start run-sml)
  '(tuareg-mode
    :start utop
    :send utop-eval-region
    )
  )
