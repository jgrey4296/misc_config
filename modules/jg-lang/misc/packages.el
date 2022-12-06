;; -*- no-byte-compile: t; -*-
;;; lang/jg-misc-lang/packages.el

(package! flycheck-plantuml)
(package! plantuml-mode)
(package! gradle-mode)
(package! groovy-mode)

(package! ob-plantuml   :recipe `(:local-repo ,(expand-file-name "packages/org-babel-ext" doom-user-dir) :files ("ob-plantuml.el")))
