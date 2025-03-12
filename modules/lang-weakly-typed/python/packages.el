;; -*- no-byte-compile: t; -*-
;;; lang/jg-python/packages.el

;; (package! python :disable t :recipe (:local-repo "/usr/local/Cellar/emacs-plus@28/28.2/share/emacs/28.2/lisp/progmodes/"))
;; (package! python :type 'built-in :disable t)
(package! python-mode :recipe (:host gitlab :repo "python-mode-devs/python-mode" :branch "master"))
(package! toml)
(package! pythonic)

(package! cython-mode)
(package! flycheck-cython)
(package! flymake-ruff)

;; Programming environment
(package! anaconda-mode)
(package! company-anaconda)
(package! company-jedi)
(package! pdb-capf)

;; Environment management
(package! pipenv)
(package! pip-requirements)
(package! pyvenv)
(package! conda)
(package! poetry)
(package! micromamba)
(package! lsp-jedi)

;; Testing frameworks
(package! python-pytest)
(package! tox)
(package! python-coverage)

;; Import managements
(package! pyimport)
(package! py-isort)
(package! pydoc)

;; babel
;; (package! ob-ipython)
(package! evil-text-object-python)
(package! evil-python-movement)
