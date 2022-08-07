;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

(package! jg-python-origami :recipe (:local-repo "~/.doom.d/packages/jg-python-origami"))
(package! python :recipe (:host github :repo "emacs-straight/python"))
(package! pipenv :disable t)
(package! pyimport :disable t)
;; (package! lsp-jedi)
;; (package! lsp-python-ms)
(package! lsp-pyright)
