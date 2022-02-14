;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

(package! jg-python-origami :recipe '(:local-repo "~/.doom.d/packages/jg-python-origami"))
(package! python-mode :built-in nil)
(package! pipenv :disable t)

(package! python :built-in nil)
(package! pyimport)
