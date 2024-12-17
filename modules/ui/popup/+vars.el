;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-popup-ivy-predicate-patterns
  (rx (or "*helpful" "*helm-" "doom" "*dired-log" "magit" "*Free Keys"))
  )

(after! ivy
  (ivy-add-actions 'ivy-switch-buffer
                   '(("p" +jg-popup-ivy-open "Popup"))
                   )
  )

(speckler-add! popup
                    '(defaults
                       ("\\*jg-customised\\*\\'" :priority -200)
                       )
                    )
