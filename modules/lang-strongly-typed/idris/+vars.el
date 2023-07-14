;;; +vars.el -*- lexical-binding: t; -*-


(spec-handling-add! lookup-regular
                    '(idris-mode
                      ("Idris Documentation" . "https://www.idris-lang.org/pages/documentation.html")
                      ("Idris Manual" . "https://idris2.readthedocs.io/en/latest/index.html")
                      )
                    )

(spec-handling-add! eval
                    `(idris-mode :start ,#'idris-pop-to-repl)
                    )

(spec-handling-add! lookup-handler
                    `(idris-mode
                      :documentation #'idris-docs-at-point
                      )
                    )
