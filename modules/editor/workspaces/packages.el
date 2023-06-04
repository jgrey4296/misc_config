;; -*- no-byte-compile: t; -*-
;;; ui/workspaces/packages.el

(package! persp-mode)
(package! window-ring-minor-mode :recipe `(:local-repo ,(expand-file-name "packages/minor-modes/window-ring-minor-mode" doom-user-dir)))
(package! project-walk :recipe (:local-repo "~/.doom.d/packages/misc/project-walk"))
(package! counsel-projectile)
(package! projectile)
(package! neotree)
