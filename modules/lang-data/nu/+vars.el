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

(spec-handling-add! file-templates
                    `(nu
                      ("\\.nu\\'" :trigger "__" :mode nushell-mode)
                      )
                    )
