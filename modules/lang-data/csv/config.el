;;; lang/data/config.el -*- lexical-binding: t; -*-

(defer-load! (jg-bindings-total csv-mode) "+bindings")

(use-package! csv-mode
  :commands csv-mode
)

(speckler-add! auto-modes ()
  '(csv
    ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
    ("\\.tsv\\'" . tsv-mode)
    )
  )
(speckler-add! treesit-lang ()
  '(csv-mode . csv)
  )
(speckler-add! treesit-source ()
  '(csv           "git@github.com:tree-sitter-grammars/tree-sitter-csv.git")
  )
