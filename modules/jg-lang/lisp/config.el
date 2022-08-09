;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(after! (evil elisp-mode)
  (load! "+bindings")
  (load! "+advice")
  )

(use-package! ffap)
(use-package! find-func)
