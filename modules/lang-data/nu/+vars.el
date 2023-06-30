;;; +vars.el -*- lexical-binding: t; -*-


(spec-handling-add! lookup-regular
                    '(nushell-mode
                      ("Reference" . "https://www.nushell.sh/book/")
                      )
                    )

(spec-handling-add! auto-modes
                    '(nu
                      ("\\.nu\\'" . nushell-mode)
                      )
                    )
