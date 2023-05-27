;;; +vars.el -*- lexical-binding: t; -*-

(after! flycheck-plantuml-executable
  (setq flycheck-plantuml-executable (executable-find "plantuml"))
  (add-to-list 'auto-mode-alist '("\\.pu" . plantuml-mode))
  )

(add-to-list 'auto-mode-alist '("\\.plantuml" . plantuml-mode))

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
