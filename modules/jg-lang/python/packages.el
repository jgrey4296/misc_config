;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

(package! jg-python-origami :recipe `(:local-repo ,(expand-file-name "packages/jg-python-origami" doom-user-dir)))
(package! python-mode :recipe (:host gitlab :repo "python-mode-devs/python-mode" :branch "master"))
;;(package! python-rx)
(package! lsp-pyright)
