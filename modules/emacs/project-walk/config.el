;;; editor/project-walk/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! evil
  (load! "+bindings")
)

(use-package! project-walk
  :commands (project-walk-minor-mode project-walk-next)
)
