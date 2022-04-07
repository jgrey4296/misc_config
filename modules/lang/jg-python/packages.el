;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

(package! jg-python-origami :recipe (:local-repo "~/.doom.d/packages/jg-python-origami"))
(package! pipenv :disable t)
(package! python :recipe (:host github :repo "emacs-straight/python"))
(package! pyimport)
;; (package! lsp-jedi)
(package! lsp-pyright)
