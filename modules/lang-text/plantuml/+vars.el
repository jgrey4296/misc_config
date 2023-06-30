;;; +vars.el -*- lexical-binding: t; -*-

(after! flycheck-plantuml-executable
  (setq flycheck-plantuml-executable (executable-find "plantuml"))
  )


(spec-handling-add! lookup-regular
                    '(plantuml-mode
                     ("Plantuml Manual" . "https://plantuml.com/")
                     )
                    )

(spec-handling-add! popup
                    '(plantuml
                      ("^\\*PLANTUML" :size 0.4 :select nil :ttl 0)
                      )
                    )

(spec-handling-add! auto-modes
                    '(plantuml
                      ("\\.plantuml" . plantuml-mode)
                      ("\\.pu" . plantuml-mode)
                      )
                    )
