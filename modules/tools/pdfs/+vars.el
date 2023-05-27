;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! popup
   '(pdf-meta
     ("^\*Pdf-Meta\*"  :side bottom :ttl nil :quit t :select nil :priority 50)
    )
   )
