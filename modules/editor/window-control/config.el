;;; editor/window-control/config.el -*- lexical-binding: t; -*-
(load! "+popup")

(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )

(load! "+funcs")
(after! evil
  (load! "+bindings")
  )
(after! ivy
  (load! "+ivy_actions")
  )


(add-hook! doom-first-input
           #'+window-control-setup-popup-rules-hook)
