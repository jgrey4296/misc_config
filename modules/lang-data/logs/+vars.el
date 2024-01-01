;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! auto-modes
                    '(logs
                      ("log\\..+\\'" . fundamental-mode)
                      ("\\.log$"     . fundamental-mode)
                      )
                    )
