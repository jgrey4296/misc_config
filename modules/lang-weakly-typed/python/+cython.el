;;; +cython.el -*- lexical-binding: t; -*-

(dlog! "Loading Cython")

(use-package! cython-mode
  :commands cython-mode
  :config
  (setq cython-default-compile-format "cython -a %s")
)

(use-package! flycheck-cython
  :after cython-mode
  )
