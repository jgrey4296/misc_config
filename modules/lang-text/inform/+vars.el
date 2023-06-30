;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! lookup-regular
                    '(inform-mode
                     ("Inform Manual" . "https://ganelson.github.io/inform/index.html")
                     )
                    )

(spec-handling-add! auto-modes
                    '(inform
                      ("\\.h\\'"   . inform-maybe-mode)
                      ("\\.inf\\'" . inform-mode)
                      )
                    )
