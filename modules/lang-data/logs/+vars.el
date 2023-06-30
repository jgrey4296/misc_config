;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! auto-modes
                    '(logs
                      ("log\\..+\\'" . jg-log-mode)
                      )
                    )
