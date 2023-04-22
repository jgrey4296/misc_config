;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode)
(package! flycheck-kotlin)
(package! gradle-mode)
(package! groovy-mode)
(package! kotlin-mode)
(when (modulep! +lsp) (package! lsp-java))
(when (modulep! +meghanada) (package! meghanada))
(package! sbt-mode)
(package! scala-mode)
(when (modulep! +lsp) (package! lsp-metals))
