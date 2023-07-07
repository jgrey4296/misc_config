;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! auto-modes
                    '(dot
                      ("\\.dot\\'" . graphviz-dot-mode)
                      ("\\.gv\\'" . graphviz-dot-mode)
                      )
                    )
