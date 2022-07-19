;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(after! (evil elisp-mode)
  (load! "+bindings")
  )
(load! "+file-templates")

(add-to-list 'auto-mode-alist '("\\.el\\.gz" . emacs-lisp-mode))
