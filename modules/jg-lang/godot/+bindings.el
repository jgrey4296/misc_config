;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map gdscript-mode-map
      :localleader
      :desc "Documentation" :n "1" (cmd! (+jg-misc-browse-url jg-godot-doc-url))

      )
