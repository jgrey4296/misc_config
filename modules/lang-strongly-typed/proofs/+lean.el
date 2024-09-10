;;; +lean.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! lean-mode :defer t)

(use-package! company-lean
  :when (modulep! :ide company)
  :after lean-mode
  :init
  (advice-add #'company-lean-hook :override #'ignore)
  )

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

(spec-handling-add! librarian-regular
                    '(lean-mode
                     ("Theorem Proving in Lean 4" . "https://leanprover.github.io/theorem_proving_in_lean4/")
                     ("Lean Manual" . "https://leanprover.github.io/lean4/doc/")
                     ("Functional Programming in Lean" . "https://leanprover.github.io/functional_programming_in_lean/")
                     ("Lean Github" . "https://github.com/leanprover/lean4")

                     )
                    )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 09, 2024
;; Modified:   September 09, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +lean.el ends here
