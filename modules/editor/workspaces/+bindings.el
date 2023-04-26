;;; +bindings.el -*- lexical-binding: t; -*-

;; Delete the current workspace if closing the last open window
(define-key! persp-mode-map
  [remap delete-window] #'+workspace/close-window-or-workspace
  [remap evil-window-delete] #'+workspace/close-window-or-workspace)
