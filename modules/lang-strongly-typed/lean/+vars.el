;;; +vars.el -*- lexical-binding: t; -*-

(sp-with-modes 'lean-mode
  (sp-local-pair "/-" "-/")
  (sp-local-pair "`" "`")
  (sp-local-pair "{" "}")
  (sp-local-pair "«" "»")
  (sp-local-pair "⟨" "⟩")
  (sp-local-pair "⟪" "⟫")
)

(spec-handling-add! lookup-handler
                    `(lean-mode
                      :definnition ,#'lean-find-definition
                      )
                    )

(spec-handling-add! company
                    '(lean-mode company-lean)
                    )

(spec-handling-add! lookup-regular
                    '(lean-mode
                     ("Theorem Proving in Lean 4" . "https://leanprover.github.io/theorem_proving_in_lean4/")
                     ("Lean Manual" . "https://leanprover.github.io/lean4/doc/")
                     ("Functional Programming in Lean" . "https://leanprover.github.io/functional_programming_in_lean/")
                     ("Lean Github" . "https://github.com/leanprover/lean4")

                     )
                    )
