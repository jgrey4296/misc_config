;;; +vars.el -*- lexical-binding: t; -*-

(after! flycheck-plantuml-executable
  (setq flycheck-plantuml-executable (executable-find "plantuml"))
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

(spec-handling-add! babel
                    '(plantuml
                      (:name plantuml :lib ob-plantuml :mode plantuml)
                      )
                    )
(spec-handling-add! org-src
                    '(plantuml
                      ("plantuml" . plantuml)
                      ("puml" . plantuml)
                      )
                    )
