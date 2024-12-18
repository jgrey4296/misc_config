;;; +vars.el -*- lexical-binding: t; -*-

(after! flycheck-plantuml-executable
  (setq flycheck-plantuml-executable (executable-find "plantuml"))
  )

(speckler-add! popup ()
  '(plantuml
    ("^\\*PLANTUML" :size 0.4 :select nil :ttl 0)
    )
  )

(speckler-add! auto-modes ()
  '(plantuml
    ("\\.plantuml" . plantuml-mode)
    ("\\.pu" . plantuml-mode)
    )
  )

(speckler-add! babel ()
  '(plantuml
    (:name plantuml :lib ob-plantuml :mode plantuml)
    )
  )
(speckler-add! org-src ()
  '(plantuml
    ("plantuml" . plantuml)
    ("puml" . plantuml)
    )
  )
