;; +bindings.el<2> -*- lexical-binding: t; -*-

(map! :leader
      :prefix "b"
      :desc "Suspend Mode"   ";" #'+jg-buffer-toggle-mode
      :desc "Scratch"        "'" (cmd! (switch-to-buffer " *temp*"))
      :desc "Popup Scratch"  "s" #'scratch-buffer
      )

(map! :map jg-binding-jump-map
      :desc "Paren State"            "p"   #'evil-parenM-state
      :desc "Jump Back"              "b"   #'better-jumper-jump-backward
      :desc "Jump Next"              "n"   #'better-jumper-jump-forward
      :desc "Jump Mark"              "m"   #'+ivy/jump-list
      )

(map! :map evil-parenM-state-map
      "a" #'evil-toggle-fold
      "z" jg-binding-vision-map
      "\\" #'+jg-text-column-motion
      "SPC" doom-leader-map
      )

(map! :map jg-binding-vision-map
      ";" #'+jg-buffer-nav-make-read-only-segment
      )
