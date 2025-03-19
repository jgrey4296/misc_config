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
  '(rst #'+jg-rst-get-commands)
  )
(speckler-add! treesit-lang ()
  '(rst-mode . rst)
  )
(speckler-add! treesit-source ()
  '(rst           "git@github.com:stsewd/tree-sitter-rst.git")
  )
