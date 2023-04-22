;;; +vars.el -*- lexical-binding: t; -*-

;;-- popup
(spec-handling-add! popup nil
   '(pdf-meta
     ("^\*Pdf-Meta\*"  :side bottom :ttl nil :quit t :select nil :priority 50)
    )
   )
;;-- end popup
