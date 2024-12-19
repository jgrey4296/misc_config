;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map plantuml-mode-map
      "|" #'librarian-insert-trigger
      )

(map! :map plantuml-mode-map
      :localleader
      )
