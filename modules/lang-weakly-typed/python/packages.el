;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

(package! jg-python-origami :recipe `(:local-repo ,(expand-file-name "packages/misc/jg-python-origami" doom-user-dir)))
(package! python-mode :recipe (:host gitlab :repo "python-mode-devs/python-mode" :branch "master"))
;; (package! python :disable t :recipe (:local-repo "/usr/local/Cellar/emacs-plus@28/28.2/share/emacs/28.2/lisp/progmodes/"))
;; (package! python :type 'built-in :disable t)
;;(package! python-rx)

(when (modulep! +cython)
  (package! cython-mode)
  (package! flycheck-cython))

;; LSP
(package! lsp-pyright)
(package! lsp-jedi      :disable t)
(package! lsp-python-ms :disable t)

;; Programming environment
(package! anaconda-mode)
(package! company-anaconda)

;; Environment management
(package! pipenv)
(package! pip-requirements)
(package! pyvenv)
(package! pyenv-mode)
(package! conda)
(package! poetry)

;; Testing frameworks
(package! nose)
(package! python-pytest)

;; Import managements
(package! pyimport)
(package! py-isort)