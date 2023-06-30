;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! auto-modes
                    '(faust
                      ("\\.dsp\\'" . faustine-mode)
                      )
                    )

(spec-handling-add! company
                    '((faust-mode faustine-mode)
                      (:mode . #'+faust-company-backend))
                    )
