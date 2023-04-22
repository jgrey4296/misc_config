;; -*- no-byte-compile: t; -*-
;;; lang/json/packages.el

(package! json-mode)
(package! json-snatcher)
(when (modulep! :completion ivy) (package! counsel-jq))
