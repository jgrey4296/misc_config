;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! popup
   '(pdf-meta
     ("^\*Pdf-Meta\*"  :side bottom :ttl nil :quit t :select nil :priority 50)
    )
   )

(speckler-add! auto-modes
                    '(pdf-meta
                      ("\\.pdf.info\\'" . pdf-meta-mode)
                      )
                    )
