;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

(package! jg-python-origami :recipe `(:local-repo ,(expand-file-name "packages/jg-python-origami" doom-user-dir)))
(package! python-mode :recipe (:host gitlab :repo "python-mode-devs/python-mode" :branch "master"))
(package! anaconda-mode)
(package! company-anaconda)

(package! pyimport)
;; (package! python :disable t :recipe (:local-repo "/usr/local/Cellar/emacs-plus@28/28.2/share/emacs/28.2/lisp/progmodes/"))
;; (package! python :type 'built-in :disable t)
(package! pyenv         :disable t)

(package! lsp-pyright)
(package! lsp-jedi      :disable t)
(package! lsp-python-ms :disable t)

;;(package! python-rx)
;;(package! pipenv        :disable t)
;;(package! pyimport      :disable t)
