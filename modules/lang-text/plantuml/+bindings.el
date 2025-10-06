;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map plantuml-mode-map

      )

(map! :map plantuml-mode-map
      :localleader
      :desc "Preview" "p" #'plantuml-preview

      )
