;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

(package! jg-python-origami :recipe `(:local-repo ,(expand-file-name "packages/jg-python-origami" doom-user-dir)))
;; (package! python :recipe `(:local-repo ,(expand-file-name  "~/github/otherLibs/lisp/emacs-src/lisp/progmodes") :files ("python.el")))
(package! pipenv :disable t)
(package! pyimport :disable t)
;; (package! lsp-jedi)
;; (package! lsp-python-ms)
(package! lsp-pyright)
