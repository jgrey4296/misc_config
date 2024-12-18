;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! evil-initial (key val)
  "Set initial evil states for modes"
  :struct '(mode evil-state)
  :loop 'do
  (evil-set-initial-state key (car (ensure-list val)))
  )
