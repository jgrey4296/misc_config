;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-popup-ivy-predicate-patterns
  (rx (or "*helpful" "*helm-" "doom" "*dired-log" "magit" "*Free Keys"))
  )

(speckler-add! ivy-actions ()
  :extend t
  '(ivy-switch-buffer
    ("p" +jg-popup-ivy-open "Popup")
    )
  )

(speckler-add! popup ()
  '(defaults
    ("\\*jg-customised\\*\\'" :priority -200)
    )
  )
