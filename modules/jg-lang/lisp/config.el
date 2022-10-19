;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(after! jg-bindings-total
  (message "Setting up lisp bindings")
  (load! "+bindings")
  (load! "+advice")
  )

(use-package! ffap)
(use-package! find-func)
