;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map gdscript-mode-map
      :localleader
      :desc "Documentation" :n "1" (cmd! (browse-url jg-godot-doc-url))

      )
