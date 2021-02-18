;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

(package! dash)
(package! s)
(package! pyenv-mode)
(package! jg-python-origami :recipe '(:local-repo "~/.doom.d/packages/jg-python-origami"))
;; (package! python-mode :recipe (:host gitlab :repo "python-mode-devs/python-mode"))
(package! python-black)
(package! blacken)

;; Major modes
(package! pip-requirements )
(when (featurep! +cython)
  (package! cython-mode )
  (when (featurep! :checkers syntax)
    (package! flycheck-cython )))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright )
      (package! lsp-python-ms ))))

;; Programming environment
(package! anaconda-mode)
(when (featurep! :completion company)
  (package! company-anaconda))

;; Environment management
(package! pipenv )
(package! pyvenv )
(when (featurep! +pyenv)
  (package! pyenv-mode ))
(when (featurep! +conda)
  (package! conda ))
(when (featurep! +poetry)
  (package! poetry ))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  )
(package! python-pytest )

;; Import managements
(package! pyimport )
(package! py-isort )
(package! cl-lib)
