;;; lang/lean/config.el -*- lexical-binding: t; -*-

(after! lean-mode
  (set-lookup-handlers! 'lean-mode
    :definition #'lean-find-definition)
  (sp-with-modes 'lean-mode
    (sp-local-pair "/-" "-/")
    (sp-local-pair "`" "`")
    (sp-local-pair "{" "}")
    (sp-local-pair "«" "»")
    (sp-local-pair "⟨" "⟩")
    (sp-local-pair "⟪" "⟫"))
  (map! :map lean-mode-map
        :localleader
        "g" #'lean-toggle-show-goal
        "n" #'lean-toggle-next-error
        (:prefix ("s" . "server")
          "r" #'lean-server-restart
          "s" #'lean-server-stop
          "v" #'lean-server-switch-version)
        (:prefix ("p" . "leanpkg")
          "t" #'lean-leanpkg-test
          "b" #'lean-leanpkg-build
          "c" #'lean-leanpkg-configure)
        "f" #'lean-fill-placeholder
        "h" #'lean-hole
        "m" #'lean-message-boxes-toggle
        "e" #'lean-execute))


(use-package! company-lean
  :when (modulep! :completion company)
  :after lean-mode
  :init
  (advice-add #'company-lean-hook :override #'ignore)
  )

(spec-handling-add! company nil
                    '(lean-mode company-lean)
                    )
(spec-handling-add! lookup-regular nil
                    '(lean-mode
                     ("Theorem Proving in Lean 4" . "https://leanprover.github.io/theorem_proving_in_lean4/")
                     ("Lean Manual" . "https://leanprover.github.io/lean4/doc/")
                     ("Functional Programming in Lean" . "https://leanprover.github.io/functional_programming_in_lean/")
                     ("Lean Github" . "https://github.com/leanprover/lean4")

                     )
                    )
