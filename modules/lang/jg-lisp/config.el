;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(after! (evil elisp-mode)
  (load! "+bindings")
  )
