;;; editor/project-walk/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

(use-package! project-walk
  :commands (project-walk-minor-mode project-walk-next)
)

(add-hook! doom-first-input #'+project-walk-binding-hook)
