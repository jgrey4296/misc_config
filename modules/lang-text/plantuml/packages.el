;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode)
(package! flycheck-plantuml)
(package! ob-plantuml   :recipe (:host github :repo "jgrey4296/misc-modes" :files ("org-babels/ob-plantuml.el") :local-repo "misc-modes"))
