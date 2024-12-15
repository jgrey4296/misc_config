;;; +vars.el -*- lexical-binding: t; -*-


(spec-handling-add! auto-modes
                    '(inform
                      ("\\.h\\'"   . inform-maybe-mode)
                      ("\\.inf\\'" . inform-mode)
                      )
                    )
