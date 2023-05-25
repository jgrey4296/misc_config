;;; +cython.el -*- lexical-binding: t; -*-

(doom-log "Loading Cython")

(use-package! cython-mode
  :when (modulep! +cython)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
)

(use-package! flycheck-cython
  :when (modulep! +cython)
  :when (modulep! :checkers syntax)
  :after cython-mode)
