;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-jump-map
      :desc "Paren State" "p"  #'evil-paren-state
      )

(map! :map evil-paren-state-map
      "a" #'evil-toggle-fold
      "z" 'jg-binding-vision-map
      )
