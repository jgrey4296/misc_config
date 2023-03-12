;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode)
(package! flycheck-plantuml)
(package! ob-plantuml   :recipe `(:local-repo ,(expand-file-name "packages/org-babel-ext" doom-user-dir) :files ("ob-plantuml.el")))
