;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-rst-mode-map (make-sparse-keymap))

(speckler-add! auto-modes ()
  '(rst
    ("\\.txt\\'" . rst-mode)
    ("\\.rst\\'" . rst-mode)
    ("\\.rest\\'" . rst-mode)
    )
  )

(speckler-add! file-templates ()
  '(rst
    ("\\.rst\\'" :trigger "__" :mode rst-mode :priority -99)

    )
  )

(speckler-add! compile-commands ()
  '(rst +jg-rst-get-commands)
  )
