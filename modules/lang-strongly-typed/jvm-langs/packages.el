;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode)
(package! flycheck-kotlin)
(package! gradle-mode)
(package! groovy-mode)
(package! kotlin-mode)
(package! lsp-java)
(when (modulep! +meghanada) (package! meghanada))
