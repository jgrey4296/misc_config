;;; editor/project-walk/config.el -*- lexical-binding: t; -*-

(use-package! project-walk
  :commands (project-walk-minor-mode project-walk-next)
)

(after! evil
  (load! "+bindings")
)
