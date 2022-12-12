;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! helpful dash)
(unpin! evil-iedit-state)
(unpin! yasnippet)
(unpin! org)

(package! python        :disable t)
(package! pyenv         :disable t)
;;(package! pipenv        :disable t)
;;(package! pyimport      :disable t)
(package! lsp-jedi      :disable t)
(package! lsp-python-ms :disable t)
