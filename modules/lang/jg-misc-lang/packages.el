;; -*- no-byte-compile: t; -*-
;;; lang/jg-misc-lang/packages.el

(package! flycheck-plantuml)
(package! plantuml-mode)
(package! gradle-mode)
(package! groovy-mode)
(package! pasp-mode)

(package! ob-plantuml   :recipe `(:local-repo "~/.doom.d/packages/org-babel-ext" :files ("ob-plantuml.el")))
