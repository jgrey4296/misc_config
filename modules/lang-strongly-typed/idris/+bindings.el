;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map idris-mode-map
      :localleader
      "r" #'idris-load-file
      "t" #'idris-type-at-point
      "d" #'idris-add-clause
      "l" #'idris-make-lemma
      "c" #'idris-case-split
      "w" #'idris-make-with-block
      "m" #'idris-add-missing
      "p" #'idris-proof-search
      "h" #'idris-docs-at-point
      )
