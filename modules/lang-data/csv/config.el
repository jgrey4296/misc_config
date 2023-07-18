;;; lang/data/config.el -*- lexical-binding: t; -*-

(defer-load! (jg-bindings-total csv-mode) "+bindings")

(use-package! csv-mode
  :commands csv-mode
)

(spec-handling-add! auto-modes
                    '(csv
                      ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
                      ("\\.tsv\\'" . tsv-mode)
                      )
                    )
