;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-rst-mode-map (make-sparse-keymap))

(spec-handling-add! auto-modes
                    '(rst
                      ("\\.txt\\'" . rst-mode)
                      ("\\.rst\\'" . rst-mode)
                      ("\\.rest\\'" . rst-mode)
                      )
                    )

(spec-handling-add! file-templates
                    '(rst
                      ("\\.rst\\'" :trigger "__" :mode rst-mode :priority -99)

                      )
                    )

(spec-handling-add! compile-commands
                    '(rst +jg-rst-get-commands)
                    )
