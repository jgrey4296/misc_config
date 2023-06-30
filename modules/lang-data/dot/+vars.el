;;; +vars.el -*- lexical-binding: t; -*-


(spec-handling-add! lookup-regular
                    '(graphviz-dot-mode
                     ("Graphviz docs" . "https://graphviz.org/doc/info/lang.html")
                     )
                    )

(spec-handling-add! auto-modes
                    '(dot
                      ("\\.dot\\'" . graphviz-dot-mode)
                      ("\\.gv\\'" . graphviz-dot-mode)
                      )
                    )
