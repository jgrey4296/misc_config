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
(spec-handling-add! lookup-regular
                    '((coq-mode proof-general)
                      ("Proof general github" . "https://github.com/ProofGeneral/PG/")
                      ("Proof General Manual" . "https://proofgeneral.github.io/doc/master/userman/")
                      ("Coq Github" . "https://github.com/coq/coq")
                      ("Coq Manual" . "https://coq.inria.fr/distrib/current/refman/")
                      ("Coq Standard Lib" . "https://coq.inria.fr/distrib/current/stdlib/")
                      ("Logical Foundations (1)" . "https://softwarefoundations.cis.upenn.edu/lf-current/toc.html")
                      ("Prog Lang Foundations (2)" . "https://softwarefoundations.cis.upenn.edu/plf-current/toc.html")
                      ("Verified Functional Algorithms (3)". "https://softwarefoundations.cis.upenn.edu/vfa-current/toc.html")
                      ("Quickchik (4)". "https://softwarefoundations.cis.upenn.edu/qc-current/toc.html")
                      ("Verified C (5)" . "https://softwarefoundations.cis.upenn.edu/vc-current/toc.html")
                      ("Separation Logic (6)" . "https://softwarefoundations.cis.upenn.edu/slf-current/toc.html")
                      )
                    )
