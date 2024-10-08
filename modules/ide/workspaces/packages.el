;; -*- no-byte-compile: t; -*-
;;; ui/workspaces/packages.el

(package! persp-mode)
(package! carousel-minor-mode :recipe (:host github :repo "jgrey4296/carousel-minor-mode"))
(package! project-zimmerframe :recipe (:host github :repo "jgrey4296/project-zimmerframe"))
(package! counsel-projectile)
(package! projectile)
(package! treemacs-projectile)
(package! related-files :recipe (:host github :repo "jgrey4296/jg-el-macros" :files ("related-files.el") :local-repo "jg-el-macros"))
(package! treemacs)
(package! winner)
