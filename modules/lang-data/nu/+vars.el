;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! auto-modes
                    '(nu
                      ("\\.nu\\'" . nushell-mode)
                      )
                    )

(speckler-add! file-templates
                    `(nu
                      ("\\.nu\\'" :trigger "__" :mode nushell-mode)
                      )
                    )
