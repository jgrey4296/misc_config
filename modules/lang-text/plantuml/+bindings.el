;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map plantuml-mode-map
      "|" #'general-insert-call
      )

(map! :map plantuml-mode-map
      :localleader
      )
