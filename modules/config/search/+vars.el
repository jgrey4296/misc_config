;;; +vars.el -*- lexical-binding: t; -*-


(speckler-add! popup ()
  '(search
    ("^\\*Fd\\*\\'" :side bottom :ttl 5 :quit t :select t :priority 50)
    )
  )
