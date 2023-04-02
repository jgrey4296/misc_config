;;; +vars.el -*- lexical-binding: t; -*-

;;-- popup
(after! jg-ui-reapply-hook-ready
  (+jg-popup-add-spec
   'pdf-meta
   '(
     ("^\*Pdf-Meta\*"  :side bottom :ttl nil :quit t :select nil :priority 50)
     )
   )
  )
;;-- end popup
