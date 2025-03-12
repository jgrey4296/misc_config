;;; +spec-defs.el -*- lexical-binding: t; -*-
(require 'macro-tools--util)

(speckler-new! evil-initial (key val)
  "Set initial evil states for modes"
  :struct '(mode evil-state)
  :loop 'do
  (evil-set-initial-state key (car (ensure-list val)))
  )

(speckler-new! evil-ex (key val)
  "Register and re-apply evil-ex cmds"
  :struct '(cmdstr . cmd)
  :loop 'do
  (cl-loop for x in val
           do
           (evil-ex-define-cmd (car x) (upfun! (cdr x)))
           )
  )
