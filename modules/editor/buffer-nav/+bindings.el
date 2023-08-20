;; +bindings.el<2> -*- lexical-binding: t; -*-


(map! :map jg-binding-jump-map
      :desc "Paren State" "p"  #'evil-paren-state
      :desc "Marks" "m" #'counsel-evil-marks
      )

(map! :map evil-paren-state-map
      "a" #'evil-toggle-fold
      "z" 'jg-binding-vision-map
      "\\" #'+jg-text-column-motion
      "SPC" doom-leader-map
      )
