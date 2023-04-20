;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode)
(package! markdown-toc)
(package! edit-indirect)

(when (modulep! +grip) (package! grip-mode))

(when (modulep! :editor evil +everywhere) (package! evil-markdown :recipe (:host github :repo "Somelauw/evil-markdown")))
