;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map plantuml-mode-map
      :localleader
      :desc "Docs: Plantuml" "1" (cmd! (browse-url "https://plantuml.com/"))
      )
