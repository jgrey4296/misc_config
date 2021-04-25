;;; editor/window-control/config.el -*- lexical-binding: t; -*-

(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )

(load! "+funcs")
(load! "+bindings")

(add-hook! doom-first-input #'+window-control-binding-hook)
