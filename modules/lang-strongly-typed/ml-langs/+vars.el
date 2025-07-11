;;; +vars.el -*- lexical-binding: t; -*-

(after! smartparens-ml
  (sp-with-modes '(tuareg-mode fsharp-mode)
    (sp-local-pair "(*" "*)" :actions nil)
    (sp-local-pair "(*" "*"
                   :actions '(insert)
                   :post-handlers '(("| " "SPC") ("|[i]*)[d-2]" "RET")))))

(speckler-add! tree-sitter-lang ()
  '(ocaml-mode         . ocaml)
  '(ocaml-ts-mode      . ocaml)
  )
(speckler-add! treesit-source ()
  '(ocaml         "git@github.com:tree-sitter/tree-sitter-ocaml.git")
  )
(speckler-add! company ()
  '(tuareg-mode merlin-company-backend)
  '(sml-mode company-mlton-grouped-backend)
  )
(speckler-add! doc-lookup ()
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
