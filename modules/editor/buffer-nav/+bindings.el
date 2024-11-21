;; +bindings.el<2> -*- lexical-binding: t; -*-

(map! :leader
      :prefix "b"
      :desc "Suspend Mode"   ";" (cmd! (major-mode-suspend))
      :desc "Restore Mopde"  "'" (cmd! (major-mode-restore))
      )

(map! :map jg-binding-jump-map
      :desc "Paren State" "p"  #'evil-parenM-state
      )

(map! :map evil-parenM-state-map
      "a" #'evil-toggle-fold
      "z" jg-binding-vision-map
      "\\" #'+jg-text-column-motion
      "SPC" doom-leader-map
      )
