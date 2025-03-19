;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! auto-modes ()
  '(dot
    ("\\.dot\\'" . graphviz-dot-mode)
    ("\\.gv\\'" . graphviz-dot-mode)
    )
  )

(speckler-add! compile-commands ()
  '(graphiz-dot-mode
    #'+jg-dot-get-commands)
  )
