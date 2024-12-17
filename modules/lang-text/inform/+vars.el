;;; +vars.el -*- lexical-binding: t; -*-


(speckler-add! auto-modes
                    '(inform
                      ("\\.h\\'"   . inform-maybe-mode)
                      ("\\.inf\\'" . inform-mode)
                      )
                    )
