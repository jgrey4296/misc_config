;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! auto-modes (key val)
  "Handler to control automodes"
  :target auto-mode-alist
  :loop 'append
  :struct '(key  val:list\[(regex . mode)\])
  val
  )
