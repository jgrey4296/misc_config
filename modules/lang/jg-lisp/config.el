;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(after! (evil elisp-mode)
  (load! "+bindings")
  )
