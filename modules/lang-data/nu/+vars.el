;;; +vars.el -*- lexical-binding: t; -*-

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
