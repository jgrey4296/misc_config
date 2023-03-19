;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map csv-mode-map
      :localleader
      "a" #'csv-align-fields
      "u" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "S" #'csv-sort-numeric-fields
      "k" #'csv-kill-fields
      "t" #'csv-transpose)
