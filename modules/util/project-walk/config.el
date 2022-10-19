;;; editor/project-walk/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
)

(use-package! project-walk
  :commands (project-walk-minor-mode project-walk-next)
)
